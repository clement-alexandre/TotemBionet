# coding: utf-8

from typing import Dict
import json

from .discrete_model import DiscreteModel
from .influence_graph import InfluenceGraph
from .gene import Gene
from .multiplex import Multiplex
from .transition import Transition


D = Dict[str, 'D']


def export_model_to_json(model: DiscreteModel) -> str:
    return json.dumps(transform_model_to_dict(model))


def export_influence_graph_to_json(influence_graph: InfluenceGraph) -> str:
    return json.dumps(transform_influence_graph_to_dict(influence_graph))


def transform_model_to_dict(model: DiscreteModel) -> D:
    return {
        'DiscreteModel': {
            'influence_graph': transform_influence_graph_to_dict(model.influence_graph),
            'transitions': [transform_transitions_to_dict(transition) for transition in model.transitions]
        }
    }


def transform_influence_graph_to_dict(influence_graph: InfluenceGraph) -> D:
    return {
        'InfluenceGraph': {
            "genes": [transform_gene_to_dict(gene) for gene in influence_graph.genes],
            "multiplexes": [transform_multiplex_to_dict(multiplex) for multiplex in influence_graph.multiplexes]
        }
    }


def transform_gene_to_dict(gene: Gene) -> D:
    return {
        'Gene': {
            "name": gene.name,
            "states": gene.states
        }
    }


def transform_multiplex_to_dict(multiplex: Multiplex) -> D:
    return {
        'Multiplex': {
            'name': multiplex.name,
            'genes': [transform_gene_to_dict(gene) for gene in multiplex.genes],
            'expression': multiplex.expression.expression
        }
    }


def transform_transitions_to_dict(transition: Transition) -> D:
    return {
        'Transition': {
            'gene': transform_gene_to_dict(transition.gene),
            'multiplexes': [transform_multiplex_to_dict(multiplex) for multiplex in transition.multiplexes],
            'states': transition.states
        }
    }
