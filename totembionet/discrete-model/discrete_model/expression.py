# coding: utf-8

from typing import Tuple
import re


class Expression:
    def __init__(self, expression: str):
        self.expression = expression

    def evaluate(self, **params: int):
        return eval(self.expression, params)

    @property
    def variables(self) -> Tuple[str, ...]:
        variables = []
        for match in re.finditer(r'(?P<name>(?!(?:not|and|or)\b)\b[a-zA-Z]\w*)', self.expression):
            variables.append(match['name'])
        return tuple(variables)

    def __str__(self):
        return self.expression
    
    def __eq__(self, other) -> bool:
        if not isinstance(other, Expression):
            return False
        return self.expression == other.expression
    
    def __hash__(self) -> int:
        return hash(self.expression)