# coding: utf-8

from typing import Tuple, List, Any


class Gene:
    def __init__(self, name: str, states: Tuple[int, ...]):
        self.name: str = name
        self.states: Tuple[int, ...] = states
        self.multiplexes: List['Multiplex'] = []

    def add_multiplex(self, multiplex: 'Multiplex') -> None:
        if multiplex not in self.multiplexes:
            self.multiplexes.append(multiplex)

    def active_multiplex(self, state: 'State') -> Tuple['Multiplex']:
        """
        Return a tuple of all the active multiplex in the given state.
        """
        return tuple(multiplex for multiplex in self.multiplexes if multiplex.is_active(state))

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, Gene):
            return False
        return self.name == other.name and self.states == other.states

    def __hash__(self) -> int:
        return hash((self.name, self.states))

    def __str__(self) -> str:
        return f'{self.name} = {" ".join(map(str, self.states))}'

    def __repr__(self) -> str:
        return self.name
