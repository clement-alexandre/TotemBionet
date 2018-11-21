# -*- coding: utf8 -*-

import random
from typing import Any, List


class ModelPickerException(Exception):
    def __init__(self, message=None, cause=None):
        self.message = message
        self.cause = cause
    
    def __str__(self):
        return self.message or str(self.cause)


def pick_a_model_randomly(models: List[Any]):
    try:
        return random.choice(models)
    except IndexError as e:
        raise ModelPickerException(cause=e)
