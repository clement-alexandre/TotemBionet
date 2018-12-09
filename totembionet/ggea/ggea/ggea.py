# -*- coding: utf8 -*-

from collections import defaultdict
from itertools import product
from typing import Any, Dict, Iterable, List, Tuple
import re
import networkx as nx
from networkx.drawing.nx_pydot import to_pydot, write_dot
from discrete_model import DiscreteModel, Gene, InfluenceGraph
import matplotlib.pyplot as plt


class Graph(nx.DiGraph):
    def __eq__(self, other):
        return nx.is_isomorphic(self, other)


def _list_to_str(lst: Iterable[Any]) -> str:
    return ''.join(map(str, lst))


def create_graph(model: DiscreteModel) -> Graph:
    genes = model.genes
    states = [gene.states for gene in genes]
    levels = product(*states)
    digraph = nx.DiGraph()
    for level in levels:
        if level:
            state = {v: level[i] for i, v in enumerate(genes)}
            for i, gene in enumerate(genes):
                next_states = model.available_state(gene, state)
                new_states = filter(lambda s: s != level[i], next_states)
                for nxt in new_states:
                    next_level = list(level)
                    next_level[i] = nxt
                    digraph.add_edge(_list_to_str(level), _list_to_str(next_level))
    return Graph(digraph)


def export_to_dot(filename: str, graph: Graph):
    write_dot(graph, filename + ".dot")


def show(graph: Graph):
    pos = nx.nx_agraph.graphviz_layout(graph)
    nx.draw_networkx_edges(graph, pos, alpha=0.3, edge_color='m')
    nx.draw_networkx_nodes(graph, pos,  node_color='w', alpha=0.4)
    nx.draw_networkx_edges(graph, pos, alpha=0.4,
                           node_size=0, width=1, edge_color='k')
    nx.draw_networkx_labels(graph, pos, fontsize=14)
    plt.show()
