# -*- coding: utf8 -*-

from collections import defaultdict, namedtuple
from typing import List, Tuple, Dict, Union, FrozenSet, Iterable

from .influence_graph import InfluenceGraph, Gene, Process



class Transition(namedtuple('Transition', 'gene process states')):
    def __str__(self):
        return f'{self.gene.name} {set(p.name for p in self.process) or "{}"} → {set(self.states) or "{}"}'


class DiscreteModel:
    def __init__(self, influence_graph: InfluenceGraph):
        self.influence_graph: InfluenceGraph = influence_graph
        self.transitions: List[Transition] = []

    @property
    def genes(self) -> Tuple[Gene]:
        return tuple(self.influence_graph.genes)
    
    @property
    def process(self) -> Tuple[Process]:
        return tuple(self.influence_graph.process)

    def find_gene_by_name(self, gene_name) -> Gene:
        return self.influence_graph.find_gene_by_name(gene_name)
    
    def find_process_by_name(self, process_name) -> Process:
        return self.influence_graph.find_process_by_name(process_name)

    def add_transition(self, gene: str, process: Tuple[str], states: Tuple[int]):
        gene = self.influence_graph.find_gene_by_name(gene)
        process = tuple(self.influence_graph.find_process_by_name(p) for p in process)
        self.transitions.append(Transition(gene, process, states))
    
    def find_transition(self, gene: Gene, process: Tuple[Process]):
        for transition in self.transitions:
            if transition.gene == gene and set(transition.process) == set(process):
                return transition
        raise AttributeError(f'transition "{gene.name} {set(p.name for p in process) or "{}"}" does not exist')

    def available_state(self, gene: Gene, states: Dict[Gene, int]) -> Tuple[int]:
        return tuple(self._available_state(gene, states))
    
    def _available_state(self, gene: Gene, states: Dict[Gene, int]) -> Iterable[int]:
        done = set()
        active_process = gene.active_process(states)
        transition = self.find_transition(gene, active_process)
        current_state = states[gene]
        for state in transition.states:
            next_state = current_state + (current_state < state) - (current_state > state)
            if next_state not in done:
                done.add(next_state)
                yield next_state

    def __str__(self):
        string = str(self.influence_graph)
        string += '\n\tmodel'
        for transition in self.list_transitions():
            string += f'\n\t\t{transition}'
        return string

    __repr__ = __str__

    def __eq__(self, other):
        if not isinstance(other, DiscreteModel):
            return False
        return (self.influence_graph == other.influence_graph
                and self.transitions == other.transitions)
