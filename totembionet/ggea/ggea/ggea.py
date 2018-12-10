# coding: utf8

from itertools import product
from typing import Any, Dict, Iterable, List, Tuple

import networkx as nx
import graphviz

from discrete_model import DiscreteModel, State, Gene, InfluenceGraph


class Graph:
    """
    The state graph for particular model.
    """
    def __init__(self, model: DiscreteModel):
        self.model = model
        self._number_of_states = 0
        self._number_of_transitions = 0
        self._graph: nx.DiGraph = self._build_graph()

    def _build_graph(self) -> nx.DiGraph:
        """ Private method to build the graph from the model. """
        digraph = nx.DiGraph()
        for state in self.model.all_states():
            self._number_of_states += 1
            for next_state in self.model.available_state(state):
                self._number_of_transitions += 1
                digraph.add_edge(
                    self._transform_state_to_string(state),
                    self._transform_state_to_string(next_state)
                )
        return digraph

    def _transform_state_to_string(self, state: State) -> str:
        """
        Private method which transform a state to a string.

        Examples
        --------

        The model contains 2 genes: operon = {0, 1, 2}
                                    mucuB = {0, 1}

        >>> graph._transform_state_to_string({operon: 1, mucuB: 0})
        "10"
        >>> graph._transform_state_to_string({operon: 2, mucuB: 1})
        "21"

        """
        return ''.join(str(state[gene]) for gene in self.model.genes)

    def _repr_svg_(self) -> str:
        """ Display the graph as html in the notebook. """
        digraph = graphviz.Digraph()
        for tail, head in self._graph.edges:
            digraph.edge(tail, head)
        return digraph._repr_svg_()
    
    def as_dot(self) -> str:
        """ Return as a string the dot version of the graph. """
        return nx.drawing.nx_pydot.to_pydot(self._graph).to_string()

    def export_to_dot(self, filename: str = 'output') -> None:
        """ Export the graph to the dot file "filename.dot". """
        with open(filename + '.dot', 'w') as output:
            output.write(self.as_dot())
    
    def number_of_states(self) -> int:
        """ Return the numbers of states in the graph """
        return self._number_of_states

    def number_of_transitions(self) -> int:
        """ Return the numbers of transitions in the graph """
        return self._number_of_transitions
