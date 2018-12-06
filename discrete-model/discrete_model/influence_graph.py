# -*- coding: utf8 -*-

from typing import List, Dict, Tuple, Set
from collections import defaultdict, namedtuple
from itertools import product
import pandas


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
        self._is_active = {}

    def is_active(self, states: Dict[Gene, int]):
        params = {gene.name: state for gene, state in states.items()
                  if gene.name in self.expression.expression}
        key = frozenset(params.items())
        if key not in self._is_active:
            self._is_active[key] = self.expression.evaluate(**params)
        return self._is_active[key]

    def __str__(self):
        return f'{self.name} : {self.expression} → {" ".join(map(repr, self.genes))}'

    def __repr__(self):
        return self.name

class ResourcesTable:
    def __init__(self, genes: Tuple[Gene], model: 'DiscreteModel' = None):
        self.genes: Tuple[Gene] = genes
        self.table = {}
        self.model = model

    def add_level(self, state: Dict[Gene, int], resouces: Tuple[Process]):
        self.table[frozenset(state.items())] = resouces

    def get_resources(self, state: Dict[Gene, int]) -> Set[Process]:
        return set(self.table[frozenset(state.items())])

    def get_resources_for_gene(self, gene: Gene, states: Dict[Gene, int]) -> Set[Process]:
        return self.get_resources(states).intersection(gene.process)

    def as_data_frame(self) -> pandas.DataFrame:
        header = defaultdict(list)
        columns = defaultdict(list)
        for state, resources in self.table.items():
            state = dict(state)
            for gene in self.genes:
                if repr(gene) not in header['genes']:
                    header['genes'].append(repr(gene))
                columns[repr(gene)].append(state[gene])
                if f"active multiplex on {gene!r}" not in header['multiplex']:
                    header['multiplex'].append(f"active multiplex on {gene!r}")
                columns[f"active multiplex on {gene!r}"].append(f'{{{", ".join(repr(p) for p in gene.process if p in resources)}}}')
                if self.model:
                    if f'K_{gene!r}' not in header['k']:
                        header['k'].append(f'K_{gene!r}')
                    columns[f'K_{gene!r}'].append(' '.join(map(str, self.model.available_state(gene, state))))
        return pandas.DataFrame(columns, columns=header['genes'] + header['multiplex'] + header['k'])

    def __str__(self):
        header = [f"active multiplex on {gene!r}" for gene in self.genes]
        result = ' | '.join(map(repr, self.genes))
        result += ' || ' + ' | '.join(header)
        if self.model:
            model_header = [f'K_{gene!r}' for gene in self.genes]
            result += ' || ' + ' | '.join(model_header)
        result += '\n' + '-' * len(result) + '\n'
        for state, resources in self.table.items():
            state = dict(state)
            columns = [f'{state[gene]:{len(repr(gene))}}' for gene in self.genes]
            result += ' | '.join(columns) + ' || '
            columns = []
            for i, gene in enumerate(self.genes):
                txt = f'{{{", ".join(repr(p) for p in gene.process if p in resources)}}}'
                columns.append(f'{txt:{len(header[i])}}')
            result += ' | '.join(columns)
            if self.model:
                result += ' || '
                txt = [' '.join(map(str, self.model.available_state(gene, state))) for gene in self.genes]
                result += ' | '.join([f'{t:{len(h)}}' for h, t in zip(model_header, txt)])
            result += '\n'
        return result


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

    def resources_table(self) -> ResourcesTable:
        resource_table = ResourcesTable(tuple(self.genes))
        levels = product(*[gene.states for gene in self.genes])
        for level in levels:
            state = {gene: level[i] for i, gene in enumerate(self.genes)}
            resources = tuple(process for process in self.process if process.is_active(state))
            resource_table.add_level(state, resources)
        return resource_table

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
