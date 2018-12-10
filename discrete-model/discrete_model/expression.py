# coding: utf-8

class Expression:
    def __init__(self, expression: str):
        self.expression = expression

    def evaluate(self, **params: int):
        return eval(self.expression, params)

    def __str__(self):
        return self.expression
    
    def __eq__(self, other) -> bool:
        if not isinstance(other, Expression):
            return False
        return self.expression == other.expression
    
    def __hash__(self) -> int:
        return hash(self.expression)