# coding: utf-8

from typing import Dict, Tuple
import json

from .discrete_model import DiscreteModel
from .influence_graph import InfluenceGraph
from .gene import Gene
from .multiplex import Multiplex
from .transition import Transition
from .expression import Expression


D = Dict[str, 'D']


def parse_json_model(string: str) -> DiscreteModel:
    return transform_dict_to_model(json.loads(string))


def parse_json_influence_graph(string: str) -> InfluenceGraph:
    return transform_dict_to_influence_graph(json.loads(string))


def transform_dict_to_model(d: D) -> DiscreteModel:
    root = d['DiscreteModel']
    influence_graph = transform_dict_to_influence_graph(root['influence_graph'])
    discrete_model = DiscreteModel(influence_graph)
    transitions = tuple(transform_dict_to_transition(transition, discrete_model) for transition in root['transitions'])
    for transition in transitions:
        discrete_model.add_transition(transition)
    return discrete_model


def transform_dict_to_influence_graph(d: D) -> InfluenceGraph:
    root = d['InfluenceGraph']
    genes = tuple(transform_dict_to_gene(gene) for gene in root['genes'])
    influence_graph = InfluenceGraph(genes)
    multiplexes = tuple(transform_dict_to_multiplex(multiplex, influence_graph) for multiplex in root['multiplexes'])
    for multiplex in multiplexes:
        influence_graph.add_multiplex(multiplex)
    return influence_graph


def transform_dict_to_transition(d: D, discrete_model: DiscreteModel) -> Transition:
    root = d['Transition']
    gene = discrete_model.find_gene_by_name(transform_dict_to_gene(root['gene']).name)
    multiplexes = tuple(discrete_model.find_multiplex_by_name(transform_dict_to_multiplex(multiplex, discrete_model.influence_graph).name) for multiplex in root['multiplexes'])
    states = tuple(root['states'])
    return Transition(gene, multiplexes, states)


def transform_dict_to_gene(d: D) -> Gene:
    root = d['Gene']
    return Gene(root['name'], tuple(root['states']))


def transform_dict_to_multiplex(d: D, influence_graph: InfluenceGraph) -> Multiplex:
    root = d['Multiplex']
    genes = tuple(influence_graph.find_gene_by_name(transform_dict_to_gene(gene).name) for gene in root['genes'])
    return Multiplex(root['name'], genes, Expression(root['expression']))