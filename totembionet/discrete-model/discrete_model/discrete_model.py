# coding: utf-8

from collections import defaultdict, namedtuple
from typing import List, Tuple, Dict, Union, FrozenSet, Iterable, Any

from .gene import Gene
from .multiplex import Multiplex
from .state import State
from .influence_graph import InfluenceGraph
from .transition import Transition


class DiscreteModel:
    def __init__(self, influence_graph: InfluenceGraph, transitions: Tuple[Transition, ...] = ()):
        self.influence_graph: InfluenceGraph = influence_graph
        self.transitions: List[Transition] = list(transitions)

    @property
    def genes(self) -> Tuple[Gene, ...]:
        """ Mapping for the InfluenceGraph.genes field. """
        return tuple(self.influence_graph.genes)
    
    @property
    def multiplexes(self) -> Tuple[Multiplex, ...]:
        """ Mapping for the InfluenceGraph.multiplexes field. """
        return tuple(self.influence_graph.multiplexes)

    def find_gene_by_name(self, gene_name: str) -> Gene:
        """ Mapping for the InfluenceGraph.find_gene_by_name method. """
        return self.influence_graph.find_gene_by_name(gene_name)
    
    def find_multiplex_by_name(self, multiplex_name: str) -> Multiplex:
        """ Mapping for the InfluenceGraph.find_multiplex_by_name method. """
        return self.influence_graph.find_multiplex_by_name(multiplex_name)

    def all_states(self) -> Tuple[State, ...]:
        """ Mapping for the InfluenceGraph.all_states method. """
        return self.influence_graph.all_states()

    def add_transition(self, transition: Transition) -> None:
        self.transitions.append(transition)
    
    def find_transition(self, gene: Gene, multiplexes: Tuple[Multiplex, ...]) -> Transition:
        """ 
        Find and return a transition in the model for the given gene and multiplexes.
        Raise an AttributeError if there is no multiplex in the graph with the given name.
        """
        multiplexes = tuple(multiplex for multiplex in multiplexes if gene in multiplex.genes)
        for transition in self.transitions:
            if transition.gene == gene and set(transition.multiplexes) == set(multiplexes):
                return transition
        raise AttributeError(f'transition K_{gene.name}' + ''.join(f"+{multiplex!r}" for multiplex in multiplexes) + ' does not exist')

    def available_state(self, state: State) -> Tuple[State, ...]:
        """ Return the state reachable from a given state. """
        result = []
        for gene in self.genes:
            result.extend(self.available_state_for_gene(gene, state))
        if len(result) > 1 and state in result:
            result.remove(state)
        return tuple(result)

    def available_state_for_gene(self, gene: Gene, state: State) -> Tuple[State, ...]:
        """ Return the state reachable from a given state for a particular gene. """
        result: List[State] = []
        active_multiplex: Tuple[Multiplex] = gene.active_multiplex(state)
        transition: Transition = self.find_transition(gene, active_multiplex)
        current_state: int = state[gene]
        done = set()
        for target_state in transition.states:
            target_state: int = self._state_after_transition(current_state, target_state)
            if target_state not in done:
                done.add(target_state)
                new_state: State = state.copy()
                new_state[gene] = target_state
                result.append(new_state)
        return tuple(result)
    
    def _state_after_transition(self, current_state: int, target_state: int) -> int:
        """
        Return the state reachable after a transition.
        Since the state for a gene can only change by 1, if the absolute value of the
        difference current_state - target_state is greater than 1, we lower it to 1 or -1.
        
        Examples
        --------

        >>> model._state_after_transition(0, 2)
        1  # Because 2 is too far from 0, the gene can only reach 1.
        >>> model._state_after_transition(1, 5)
        2  # 5 is still is too far from 1, so the gene can only reach 2.
        >>> model._state_after_transition(2, 1)
        1  # No problem here, 1 is at distance 1 from 2
        >>> model._state_after_transition(1, 1)
        1  # The state does not change here

        """
        return current_state + (current_state < target_state) - (current_state > target_state)

    def __str__(self) -> str:
        string = str(self.influence_graph)
        string += '\n\tmodel'
        for transition in self.transitions:
            string += f'\n\t\t{transition}'
        return string

    def __repr__(self) -> str:
        return f"DiscreteModel(influence graph={self.influence_graph!r}, transitions={self.transitions})"

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, DiscreteModel):
            return False
        return (self.influence_graph == other.influence_graph
                and self.transitions == other.transitions)
