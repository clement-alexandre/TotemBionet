# coding: utf-8

from typing import Tuple, Dict
from collections import defaultdict

import pandas

from discrete_model import DiscreteModel, InfluenceGraph, Multiplex, State, Gene


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
    
    def _repr_html_(self):
        return self.as_data_frame()._repr_html_()
    
    def _repr_multiplexes(self, gene: Gene, state: State) -> str:
        active_multiplexes = self.active_multiplexes_for_gene(gene, state)
        return f'{{{", ".join(map(repr, active_multiplexes))}}}'
