# -*- coding: utf8 -*-

from collections import defaultdict
from itertools import product
from typing import Any, Dict, Iterable, List, Tuple
import re
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
        self.multiplex = multiplex or defaultdict(dict)

    def __eq__(self, other):
        if not isinstance(other, GGEAModel):
            return False
        return (self.variables == other.variables
                and self.relations == other.relations
                and self.multiplex == other.multiplex)


class Graph(nx.DiGraph):
    def __eq__(self, other):
        return nx.is_isomorphic(self, other)


def _list_to_str(lst: Iterable[Any]) -> str:
    return ''.join(map(str, lst))


def _assign_k(model: GGEAModel, state: State, var: str) -> str:
    k = "K_%s" % var
    if not var in model.multiplex:
        return k
    for multiplex, expression in sorted(model.multiplex[var].items()):
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


def _parse_variables(lines: List[str]) -> Variables:
    variables: Variables = {}
    while lines and not re.match(r'^\s*REG\s*$', lines[-1]):
        match = re.match(r'^(\w+) = ((?:\w+\s+)+);$', lines.pop())
        if match:
            variables[match[1]] = list(map(int, match[2].split()))
    return variables


def _parse_multiplex(lines: List[str]) -> Mutliplex:
    mutliplex: Mutliplex = defaultdict(dict)
    while lines and not re.match(r'^\s*PARA\s*$', lines[-1]):
        match = re.match(r'^(\w+)\s+\[\((.*?)\)\]\s*=>\s*((?:\w+\s+)+)\s*;$', lines.pop())
        if match:
            for variable in match[3].split():
                mutliplex[variable][match[1]] = _convert_to_python(match[2])
    return mutliplex


def _convert_to_python(expression: str):
    for c, token in (('!', ' not '), ('&', ' and '), ('|', ' or '), ('=', ' == '),
                     ('<', ' < '), ('>', ' > '), ('<  ==', ' <= '), ('>  ==', ' >= ')):
        expression = expression.replace(c, token)
    return expression


def _parse_parameters(lines: List[str]):
    relations = defaultdict(dict)
    while lines and not re.match(r'^\s*(?:# NO CTL FORMULA|CTL)\s*$', lines[-1]):
        match = re.match(r'^# Parameters for (\w+)\s*$', lines.pop())
        if match:
            variable = match[1]
            while (lines 
                   and not re.match(r'^# Parameters for (\w+)\s*$', lines[-1]) 
                   and not re.match(r'^\s*(?:# NO CTL FORMULA|CTL)\s*$', lines[-1])):
                    match = re.match(r'^(.+?) = ((?:\w+\s+)+);$', lines.pop())
                    if match:
                        relations[variable][match[1]] = list(map(int, match[2].split()))
    return relations


def _parse_models(lines: List[str]):
    models = []
    while lines:
        match = re.match(r'^# MODEL (\d+)\s*$', lines.pop())
        if match:
            tmp = [{}]
            while lines and not re.match(r'^# MODEL (\d+)\s*$', lines[-1]):
                    match = re.match(r'^# (.+?) = ((?:\d+\s*)+)$', lines.pop())
                    if match:
                        tmp2 = []
                        for model in tmp:
                            for value in match[2].split():
                                copy = model.copy()
                                copy[match[1]] = int(value)
                                tmp2.append(copy)
                        tmp = tmp2
            models.extend(tmp)
    return models


def parse(string: str) -> Iterable[GGEAModel]:
    lines = string.splitlines()[::-1]
    variables = _parse_variables(lines)
    multiplex = _parse_multiplex(lines)
    parameters = _parse_parameters(lines)
    models = _parse_models(lines)
    for model in models:
        relations = {k: {_k: model[_k] for _k in v} for k, v in parameters.items()}
        yield GGEAModel(variables, relations, multiplex)


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
    import matplotlib.pyplot as plt
    pos = nx.nx_agraph.graphviz_layout(graph)
    nx.draw_networkx_edges(graph, pos, alpha=0.3, edge_color='m')
    nx.draw_networkx_nodes(graph, pos,  node_color='w', alpha=0.4)
    nx.draw_networkx_edges(graph, pos, alpha=0.4, node_size=0, width=1, edge_color='k')
    nx.draw_networkx_labels(graph, pos, fontsize=14)
