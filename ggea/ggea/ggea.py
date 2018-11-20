# -*- coding: utf8 -*-

from collections import defaultdict
from itertools import product
from typing import Any, Dict, Iterable, List, Tuple

import networkx as nx
from networkx.drawing.nx_pydot import to_pydot, write_dot

Variables = Dict[str, List[int]]
Relations = Dict[str, Dict[str, int]]
Mutliplex = Dict[str, Dict[str, str]]
State = Dict[str, int]
Level = Tuple[List[int]]
FP = Dict[str, int]
DFP = Dict[str, int]


class GGEAModel:
    def __init__(self, variables: Variables = None,
                 relations: Relations = None, multiplex: Mutliplex = None):
        self.variables = variables or defaultdict(list)
        self.relations = relations or defaultdict(dict)
        self.multiplex = multiplex or defaultdict(list)


class Graph(nx.DiGraph):
    def __eq__(self, other):
        return nx.is_isomorphic(self, other)


def _list_to_str(lst: Iterable[Any]) -> str:
    return '"' + ''.join(map(str, lst)) + '"'


def _assign_k(model: GGEAModel, state: State, var: str) -> str:
    k = "K_%s" % var
    for multiplex, expression in model.multiplex[var].items():
        if not expression or eval(expression, state.copy()):
            k = k + "+%s" % multiplex
    return k


def _fp(model: GGEAModel, s: State) -> FP:
    return {var: model.relations[var][_assign_k(model, s, var)] for var in s}


def _dfp(model: GGEAModel, s: State) -> DFP:
    fp = _fp(model, s)
    return {v: s[v] + (s[v] < fp[v]) - (s[v] > fp[v]) for v in s}


def _state(relations: Relations, level: Level) -> State:
    return {k: v for k, v in zip(relations, level)}


def create_graph(model: GGEAModel) -> Graph:
    levels = product(*model.variables.values())
    digraph = nx.DiGraph()
    for level in levels:
        if level:
            state = _state(model.relations, level)
            digraph.add_edge(_list_to_str(state.values()),
                             _list_to_str(_dfp(model, state).values()))
    return Graph(digraph)


def export_to_dot(filename: str, graph: Graph):
    write_dot(graph, filename + ".dot")


def show(graph: Graph):
    from IPython.display import Image, display
    plt = Image(to_pydot(graph).create_png())
    display(plt)
