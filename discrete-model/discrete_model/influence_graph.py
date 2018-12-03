# -*- coding: utf8 -*-

from typing import List, Dict, Tuple, Set
from collections import defaultdict, namedtuple


class Gene:
    def __init__(self, name, states):
        self.name = name
        self.states = states
        self.process = []

    def add_process(self, process: 'Process'):
        if process not in self.process:
            self.process.append(process)
    
    def active_process(self, state: Dict['Gene', int]) -> Tuple['Process']:
        return tuple(process for process in self.process if process.is_active(state))

    def __eq__(self, other):
        if not isinstance(other, Gene):
            return False
        return self.name == other.name and self.states == other.states
    
    def __hash__(self):
        return hash((self.name, self.states))

    def __str__(self):
        return f'{self.name} = {" ".join(map(str, self.states))}'
    
    def __repr__(self):
        return self.name


class Expression(namedtuple('Expression', 'expression')):
    def evaluate(self, **params: int):
        return eval(self.expression, params)
    
    def __str__(self):
        return self.expression


class Process(namedtuple('Process', 'name genes expression')):
    def __init__(self, name: str, genes: Tuple[Gene], expression: Expression):
        for gene in genes:
            gene.add_process(self)

    def is_active(self, states: Dict[Gene, int]):
        params = {gene.name: state for gene, state in states.items()}
        return self.expression.evaluate(**params)

    def __str__(self):
        return f'{self.name} : {self.expression} → {" ".join([gene.name for gene in self.genes])}'


class InfluenceGraph:
    def __init__(self):
        self.genes: List[Gene] = []
        self.process: List[Process] = []

    def add_gene(self, name: str, state_min: int, state_max: int):
        self.genes.append(Gene(name, tuple(range(state_min, state_max+1))))

    def find_gene_by_name(self, gene_name: str) -> Gene:
        for gene in self.genes:
            if gene.name == gene_name:
                return gene
        raise AttributeError(f'gene "{gene_name}" does not exist')

    def list_genes(self) -> Tuple[Gene]:
        return tuple(self.genes)

    def add_process(self, name: str, expression: str, *genes: str):
        genes = tuple(self.find_gene_by_name(gene) for gene in genes)
        process = Process(name, genes, Expression(expression))
        self.process.append(process)

    def find_process_by_name(self, process_name: str) -> Process:
        for process in self.process:
            if process.name == process_name:
                return process
        raise AttributeError(f'process "{process_name}" does not exist')

    def list_process(self) -> Tuple[Process]:
        return tuple(self.process)

    def __str__(self):
        string = '\nInfluenceGraph\n\tgenes:\n\t\t'
        string += '\n\t\t'.join(map(str, self.list_genes()))
        string += '\n\tprocess\n\t\t'
        string += '\n\t\t'.join(map(str, self.list_process()))
        return string

    __repr__ = __str__

    def __eq__(self, other):
        if not isinstance(other, InfluenceGraph):
            return False
        return (self.genes == other.genes
                and self.process == other.process)
