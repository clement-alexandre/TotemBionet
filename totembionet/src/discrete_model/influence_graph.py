# coding: utf-8

from typing import List, Dict, Tuple, Set, Any
from collections import defaultdict, namedtuple
from itertools import product
import html

import graphviz

from .gene import Gene
from .multiplex import Multiplex
from .state import State


class InfluenceGraph:
    def __init__(self, genes: Tuple[Gene, ...] = (), multiplexes: Tuple[Multiplex, ...] = ()):
        self.genes: List[Gene] = list(genes)
        self.multiplexes: List[Multiplex] = list(multiplexes)

    def add_gene(self, gene: Gene) -> None:
        """ Add a gene to the influence graph. """
        self.genes.append(gene)

    def find_gene_by_name(self, gene_name: str) -> Gene:
        """ 
        Find and return a gene in the influence graph with the given name.
        Raise an AttributeError if there is no gene in the graph with the given name.
        """
        for gene in self.genes:
            if gene.name == gene_name:
                return gene
        raise AttributeError(f'gene "{gene_name}" does not exist')

    def add_multiplex(self, multiplex: Multiplex) -> None:
        """ Add a multiplex to the influence graph. """
        self.multiplexes.append(multiplex)

    def find_multiplex_by_name(self, multiplex_name: str) -> Multiplex:
        """ 
        Find and return a multiplex in the influence graph with the given name.
        Raise an AttributeError if there is no multiplex in the graph with the given name.
        """
        for multiplex in self.multiplexes:
            if multiplex.name == multiplex_name:
                return multiplex
        raise AttributeError(f'multiplex "{multiplex_name}" does not exist')
    
    def all_states(self) -> Tuple[State, ...]:
        """ Return all the possible states of this influence graph. """
        return tuple(self._transform_list_of_states_to_state(states)
                     for states in self._cartesian_product_of_every_states_of_each_genes())

    def _cartesian_product_of_every_states_of_each_genes(self) -> Tuple[Tuple[int, ...]]:
        """ 
        Private method which return the cartesian product of the states
        of the genes in the model. It represents all the possible state for a given model.

        Examples
        --------

        The model contains 2 genes: operon = {0, 1, 2}
                                    mucuB = {0, 1}
        Then this method returns ((0, 0), (0, 1), (1, 0), (1, 1), (2, 0), (2, 1))
        For reach tuple, the first element is the state of the operon gene, and the
        second element stands for the state of the mucuB gene.
        """
        if not self.genes:
            return () 
        return tuple(product(*[gene.states for gene in self.genes]))

    def _transform_list_of_states_to_state(self, state: List[int]) -> State:
        """ 
        Private method which transform a list which contains the state of the gene
        in the models to a State object.

        Examples
        --------

        The model contains 2 genes: operon = {0, 1, 2}
                                    mucuB = {0, 1}
        >>> graph._transform_list_of_states_to_dict_of_states([0, 1])
        {operon: 0, mucuB: 1}
        >>> graph._transform_list_of_states_to_dict_of_states([2, 0])
        {operon: 2, mucuB: 0}
        """
        return State({gene: state[i] for i, gene in enumerate(self.genes)})

    def show(self, engine="fdp") -> 'InfluenceGraphDisplayer':
        """ 
        Display the graph using one of the graphviz engine.
        Available engines: 'dot', 'twopi', 'fdp', 'patchwork',
                           'neato', 'osage', 'circo', 'sfdp'.
        """
        return InfluenceGraphDisplayer(self, engine)
    
    def __str__(self) -> str:
        string = '\nInfluenceGraph\n\tgenes:\n\t\t'
        string += '\n\t\t'.join(map(str, self.genes))
        string += '\n\tmultiplex\n\t\t'
        string += '\n\t\t'.join(map(str, self.multiplexes))
        return string

    def __repr__(self) -> str:
        return f'InfluenceGraph(genes={self.genes}, multiplexes={self.multiplexes})'

    def _repr_svg_(self) -> str:
        """ Display the graph as html in the notebook. """
        return self.show()._repr_svg_()

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, InfluenceGraph):
            return False
        return (set(self.genes) == set(other.genes)
                and set(self.multiplexes) == set(other.multiplexes))

    def __hash__(self) -> int:
        return hash((frozenset(self.genes), frozenset(self.multiplexes)))


class InfluenceGraphDisplayer:
    def __init__(self, influence_graph: InfluenceGraph, engine: str):
        self.influence_graph = influence_graph
        self.engine = engine
        self.digraph = graphviz.Digraph(engine=engine)
        for gene in influence_graph.genes:
            self.digraph.node(gene.name)
        for multiplex in influence_graph.multiplexes:
            self.digraph.node(multiplex.name, self._multiplex_to_html(multiplex), shape="plaintext")
            for gene in multiplex.genes:
                self.digraph.edge(multiplex.name, gene.name)
            for gene in multiplex.expression.variables:
                self.digraph.edge(gene, multiplex.name)
    
    def _multiplex_to_html(self, multiplex: Multiplex) -> str:
        return f'''<<table border="0" cellborder="1" cellspacing="0">
            <tr><td><font color="blue">{multiplex.name}</font></td></tr>
            <tr><td>{html.escape(multiplex.expression.expression)}</td></tr></table>>'''

    def _repr_svg_(self) -> str:
        """ Display the graph as html in the notebook. """
        return self.digraph._repr_svg_()