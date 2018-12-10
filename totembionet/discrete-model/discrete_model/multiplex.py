# coding: utf-8

from typing import Tuple, Dict, FrozenSet

from .gene import Gene
from .state import State
from .expression import Expression


class Multiplex:
    def __init__(self, name: str, genes: Tuple[Gene], expression: Expression):
        self.name: str = name
        self.genes: Tuple[Gene] = genes
        self.expression = expression
        for gene in genes:
            gene.add_multiplex(self)

        # a cache table to store the already evaluated states with their results.
        self._is_active: Dict[State, bool] = {}

    def is_active(self, state: 'State') -> bool:
        """ Return True if the multiplex is active in the given state, false otherwise. """
        # Remove the genes which does not contribute to the multiplex
        sub_state = state.sub_state_by_gene_name(*self.expression.variables)
        # If this state is not in the cache
        if sub_state not in self._is_active:
            params = self._transform_state_to_dict(sub_state)
            # We add the result of the expression for this state of the multiplex to the cache
            self._is_active[sub_state] = self.expression.evaluate(**params)
        return self._is_active[sub_state]

    def _transform_state_to_dict(self, state: 'State') -> Dict[str, int]:
        return {gene.name: state for gene, state in state.items()
                if gene.name in self.expression.expression}

    def __str__(self) -> str:
        return f'{self.name} : {self.expression} → {" ".join(map(repr, self.genes))}'

    def __repr__(self) -> str:
        return self.name
    
    def __eq__(self, other) -> bool:
        if not isinstance(other, Multiplex):
            return False
        return (self.name == other.name 
                and self.expression == other.expression
                and set(self.genes) == set(other.genes))

    def __hash__(self) -> int:
        return hash((self.name, frozenset(self.genes), self.expression))