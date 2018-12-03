# -*- coding: utf8 -*-

from collections import defaultdict, namedtuple
from typing import List, Tuple, Dict, Union, FrozenSet

from .influence_graph import InfluenceGraph, Variable, Process



class Transition(namedtuple('Transition', 'gene process states')):
    def __str__(self):
        return f'{self.gene.name} {set(p.name for p in self.process) or "{}"} → {set(self.states) or "{}"}'


class DiscreteModel:
    def __init__(self, influence_graph: InfluenceGraph):
        self.influence_graph: InfluenceGraph = influence_graph
        self.transitions: List[Transition] = []

    def add_transition(self, gene: str, process: Tuple[str], states: Tuple[int]):
        gene = self.influence_graph.find_variable_by_name(gene)
        process = tuple(self.influence_graph.find_process_by_name(p) for p in process)
        self.transitions.append(Transition(gene, process, states))
    
    def find_transition(self, gene: Variable, process: Tuple[Process]):
        for transition in self.transitions:
            if transition.gene == gene and set(transition.process) == set(process):
                return transition
        raise AttributeError(f'transition "{gene.name} {set(p.name for p in process) or "{}"}" does not exist')

    def list_transitions(self) -> Tuple[Transition]:
        return tuple(self.transitions)

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
