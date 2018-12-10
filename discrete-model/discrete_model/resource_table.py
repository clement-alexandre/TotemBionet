# coding: utf-8

from typing import Tuple, Dict
from collections import defaultdict

import pandas

from .gene import Gene
from .state import State
from .transition import Transition
from .influence_graph import InfluenceGraph
from .multiplex import Multiplex
from .discrete_model import DiscreteModel


class ResourceTable:
    """
    Create the resource table from the influence graph.
    """
    def __init__(self, influence_graph: InfluenceGraph):
        self.influence_graph: InfluenceGraph = influence_graph
        self.table: Dict[State, Tuple[Multiplex, ...]] = self._build_table()
    
    def _build_table(self) -> Dict[State, Tuple[Multiplex, ...]]:
        """ Private method which build the table which map a State to the active multiplex. """
        result: Dict[State, Tuple[Multiplex, ...]] = {}
        for state in self.influence_graph.all_states():
            result[state] = tuple(multiplex for multiplex in self.influence_graph.multiplexes 
                                  if multiplex.is_active(state))
        return result
    
    def active_multiplexes_for_gene(self, gene: Gene, state: State) -> Tuple[Multiplex, ...]:
        return tuple(multiplex for multiplex in self.table[state] if multiplex in gene.multiplexes)

    def as_data_frame(self) -> pandas.DataFrame:
        """ Create a panda DataFrame representation of the resource table. """
        header_gene = {}
        header_multiplex = {}
        for gene in self.influence_graph.genes:
            header_gene[gene] = repr(gene)
            header_multiplex[gene] = f"active multiplex on {gene!r}"
        
        columns = defaultdict(list)
        for state in self.table.keys():
            for gene in self.influence_graph.genes:
                columns[header_gene[gene]].append(state[gene])
                columns[header_multiplex[gene]].append(self._repr_multiplexes(gene, state))
        
        header = list(header_gene.values()) + list(header_multiplex.values())
        return pandas.DataFrame(columns, columns=header)
    
    def _repr_multiplexes(self, gene: Gene, state: State) -> str:
        active_multiplexes = self.active_multiplexes_for_gene(gene, state)
        return f'{{{", ".join(map(repr, active_multiplexes))}}}'



class ResourceTableWithModel(ResourceTable):
    def __init__(self, model: DiscreteModel):
        super().__init__(model.influence_graph)
        self.model = model
        self.transition_table: Dict[Tuple[Gene, State], Transition] = self._build_transition_table()
    
    def _build_transition_table(self) -> Dict[Tuple[Gene, State], Transition]:
        result = {}
        for state, multiplexes in self.table.items():
            for gene in self.model.genes:
                result[gene, state] = self.model.find_transition(gene, multiplexes)
        return result
        
    def as_data_frame(self) -> pandas.DataFrame:
        """ Create a panda DataFrame representation of the resource table. """
        header_gene = {}
        header_multiplex = {}
        headr_transitions = {}
        for gene in self.influence_graph.genes:
            header_gene[gene] = repr(gene)
            header_multiplex[gene] = f"active multiplex on {gene!r}"
            headr_transitions[gene] = f"K_{gene!r}"
        
        columns = defaultdict(list)
        for state in self.table.keys():
            for gene in self.influence_graph.genes:
                columns[header_gene[gene]].append(state[gene])
                columns[header_multiplex[gene]].append(self._repr_multiplexes(gene, state))
                columns[headr_transitions[gene]].append(self._repr_transition(gene, state))

        header = list(header_gene.values()) + list(header_multiplex.values()) + list(headr_transitions.values())
        return pandas.DataFrame(columns, columns=header)

    def _repr_transition(self, gene: Gene, state: State) -> str:
        transition = self.transition_table[gene, state]
        return f'{" ".join(map(str, transition.states))}'

