# coding: utf-8

import collections
from typing import Tuple, Iterable, Mapping

from .gene import Gene


class State(collections.UserDict):
    """
    Represent a state of the system at a certain time
    i.e for each gene, we assign an integer which represent its level.

    Examples
    --------

    >>> state = State()
    >>> state[operon] = 2
    >>> state[mucuB] = 0
    >>> state[operon]
    2
    >>> for gene, level in state:
    ...     print(gene, level)
    operon 2
    mucuB 0
    """
    def __init__(self, initial_data: Mapping[Gene, int] = {}):
        super().__init__()
        for gene, state in initial_data.items():
            self[gene] = state

    def __getitem__(self, gene: Gene) -> int:
        return self.data[gene]
    
    def __iter__(self) -> Iterable[Gene]:
        yield from self.data
        
    def __len__(self) -> int:
        return len(self.data)

    def sub_state_by_gene_name(self, *gene_names: str) -> 'State':
        """
        Create a sub state with only the gene passed in arguments.

        Example
        -------

        >>> state.sub_state_by_gene_name('operon')
        {operon: 2}
        >>> state.sub_state_by_gene_name('mucuB')
        {mucuB: 0}
        
        """
        return State({gene: state for gene, state in self.items() if gene.name in gene_names})

    def __str__(self) -> str:
        return str(self.data)
    
    def __repr__(self) -> str:
        return repr(self.data)

    def __hash__(self) -> int:
        return hash(frozenset(self.items()))
    
    def copy(self) -> 'State':
        return State(self)
