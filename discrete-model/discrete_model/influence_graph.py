# -*- coding: utf8 -*-

from typing import List, Dict, Tuple, Set
from collections import defaultdict, namedtuple
from dataclasses import dataclass


class Variable:
    def __init__(self, name, states):
        self.name = name
        self.states = states
        self.process = []

    def add_process(self, process: 'Process'):
        if process not in self.process:
            self.process.append(process)

    def __eq__(self, other):
        if not isinstance(other, Variable):
            return False
        return self.name == other.name and self.states == other.states
    
    def __hash__(self):
        return hash((self.name, self.states))

    def __str__(self):
        return f'{self.name} = {" ".join(map(str, self.states))}'
    
    def __repr__(self):
        return self.name


class Expression(namedtuple('Expression', 'expression')):
    def evaluate(self, **params):
        return eval(self.expression, params)
    
    def __str__(self):
        return self.expression


class Process(namedtuple('Process', 'name genes expression')):
    def __init__(self, name: str, genes: Tuple[Variable], expression: Expression):
        for gene in genes:
            gene.add_process(self)

    def is_active(self, **states):
        return self.expression.evaluate(**states)

    def __str__(self):
        return f'{self.name} : {self.expression} → {" ".join([gene.name for gene in self.genes])}'


class InfluenceGraph:
    def __init__(self):
        self.variables: List[Variable] = []
        self.process: List[Process] = []

    def add_variable(self, name: str, state_min: int, state_max: int):
        self.variables.append(
            Variable(name, tuple(range(state_min, state_max+1))))

    def find_variable_by_name(self, variable_name: str) -> Variable:
        for variable in self.variables:
            if variable.name == variable_name:
                return variable
        raise AttributeError(f'variable "{variable_name}" does not exist')

    def list_variables(self) -> Tuple[Variable]:
        return tuple(self.variables)

    def add_process(self, name: str, expression: str, *genes: str):
        variables = tuple(self.find_variable_by_name(gene) for gene in genes)
        process = Process(name, variables, Expression(expression))
        self.process.append(process)

    def find_process_by_name(self, process_name: str) -> Process:
        for process in self.process:
            if process.name == process_name:
                return process
        raise AttributeError(f'process "{process_name}" does not exist')

    def list_process(self) -> Tuple[Process]:
        return tuple(self.process)

    def __str__(self):
        string = '\nInfluenceGraph\n\tvariables:\n\t\t'
        string += '\n\t\t'.join(map(str, self.list_variables()))
        string += '\n\tprocess\n\t\t'
        string += '\n\t\t'.join(map(str, self.list_process()))
        return string

    __repr__ = __str__

    def __eq__(self, other):
        if not isinstance(other, InfluenceGraph):
            return False
        return (self.variables == other.variables
                and self.process == other.process)
