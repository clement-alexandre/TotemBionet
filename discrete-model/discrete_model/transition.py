# coding: utf-8

from typing import Tuple

from .gene import Gene
from .multiplex import Multiplex


class Transition:
    def __init__(self, gene: Gene, multiplexes: Tuple[Multiplex, ...], states: Tuple[int, ...]):
        self.gene: Gene = gene
        self.multiplexes: Tuple[Multiplex, ...] = multiplexes
        self.states: Tuple[int, ...] = states

    def __str__(self) -> str:
        return f'{self.gene!r} {{{", ".join(map(repr, self.multiplexes))}}} → {{{", ".join(map(str, self.states))}}}'

    def __repr__(self) -> str:
        return f'K_{self.gene!r}' + ''.join(f'+{multiplex!r}' for multiplex in self.multiplexes)
    
    def __eq__(self, other) -> bool:
        if not isinstance(other, Transition):
            return False
        return (self.gene == other.gene
                and set(self.multiplexes) == set(other.multiplexes)
                and self.states == other.states)
            
    def __hash__(self) -> int:
        return hash((self.gene, frozenset(self.multiplexes), self.states))
