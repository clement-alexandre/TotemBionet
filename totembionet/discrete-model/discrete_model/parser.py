# coding: utf-8

from typing import List, Iterable, Tuple
import re

from .gene import Gene
from .multiplex import Multiplex
from .expression import Expression
from .influence_graph import InfluenceGraph
from .transition import Transition
from .discrete_model import DiscreteModel


def parse_smbionet_output_file(filename: str) -> Tuple[DiscreteModel, ...]:
    with open(filename) as output:
        return parse_smbionet_output_string(output.read())


def parse_smbionet_output_string(string: str) -> Tuple[DiscreteModel, ...]:
    graph = InfluenceGraph()
    match = _create_pattern().match(string)
    _parse_genes(match['genes'], graph)
    _parse_multiplex(match['multiplex'], graph)
    return tuple(_parse_models(match['models'], graph))


def _create_pattern():
    read_genes = r'\s*VAR(?P<genes>(?:.|\s)*?)'
    read_multiplex = r'REG(?P<multiplex>(?:.|\s)*?)'
    read_parameters = r'(?:PARA(?P<parameters>(?:.|\s)*?))?'
    read_ctl = r'(?:CTL(?P<ctl>(?:.|\s)*?))?'
    read_models = r'(?P<models>#(?:.|\s)*?)'
    read_check = r'(?P<checks>#\s*SELECTED MODELS(?:.|\s)*?)?'
    return re.compile(
        '^'
        + read_genes
        + read_multiplex
        + read_parameters
        + read_ctl
        + read_models
        + read_check
        + '$'
    )


def _parse_genes(string: str, graph: InfluenceGraph) -> None:
    pattern = re.compile(r'\s*(?P<gene>\w+)\s*=\s*(?P<start>\d+)\s+(?P<end>\d+)\s*;')
    for match in pattern.finditer(string):
        states = tuple(range(int(match['start']), int(match['end'])+1))
        gene = Gene(match['gene'], states)
        graph.add_gene(gene)


def _parse_multiplex(string: str, graph: InfluenceGraph) -> None:
    pattern = re.compile(r'(?P<name>\w+)\s+\[(?P<predicate>.*?)\]\s*=>\s*(?P<genes>(?:\w+\s*)+)\s*;')
    for match in pattern.finditer(string):
        genes = tuple(graph.find_gene_by_name(gene_name) for gene_name in match['genes'].split())
        multiplex = Multiplex(match['name'], genes, _parse_predicate(match['predicate']))
        graph.add_multiplex(multiplex)


def _parse_predicate(predicate: str) -> Expression:
    predicate = re.sub(r'(?<![<>])=', '==', predicate)
    predicate = re.sub(r'!', 'not', predicate)
    predicate = re.sub(r'\|', 'or', predicate)
    predicate = re.sub(r'&', 'and', predicate)
    return Expression(predicate)


def _parse_models(string: str, graph: InfluenceGraph) -> Iterable[DiscreteModel]:
    pattern = re.compile(r'#\s+MODEL\s+(?P<id>\d+)\s+(?P<k_assigments>(?:(?!\s*#\s*MODEL).*\s*)*)')
    for match in pattern.finditer(string):
        yield _parse_k_assigments(match['k_assigments'], graph)


def _parse_k_assigments(string: str, graph: InfluenceGraph) -> DiscreteModel:
    model = DiscreteModel(graph)
    pattern = re.compile(r'#\s*K_(?P<gene>[^\s=]+)\s*=\s*(?P<states>(?:\d+\s+)*?\d+)\s*$', flags=re.MULTILINE)
    for match in pattern.finditer(string):
        gene, *multiplexes = match['gene'].split('+')
        gene = graph.find_gene_by_name(gene)
        multiplexes = tuple(graph.find_multiplex_by_name(multiplex) for multiplex in multiplexes)
        states = tuple(map(int, match['states'].split()))
        transition = Transition(gene, multiplexes, states)
        model.add_transition(transition)
    return model
