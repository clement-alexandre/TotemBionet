# -*- coding: utf8 -*-

import random
from typing import Dict, List
from discrete_model import DiscreteModel, InfluenceGraph, Variable, Transition
import matplotlib.pyplot as plt
import numpy as np


class Simulation:
    def __init__(self, model: DiscreteModel):
        self.model = model
        self.random = random.Random()
        self.steps = 100
        self.initial_state = None

    def run(self) -> 'Result':
        states = [self.initial_state or {
            variable: self.random.choice(variable.states)
            for variable in self.model.influence_graph.list_variables()
        }]
        for _ in range(self.steps-1):
            states.append(self._next_step(states[-1]))
        return Result(self, states)

    def _next_step(self, states: Dict[Variable, int]):
        return {variable: self.random.choice(self._future(variable, states))
                for variable, state in states.items()}

    def _future(self, variable, states):
        active_process = tuple(process for process in variable.process
                               if process.is_active(**{v.name: s for v, s in states.items()}))
        transition = self.model.find_transition(variable, tuple(active_process))
        return transition.states


class Result:
    def __init__(self, simulation: Simulation, states: List[Dict[Variable, int]]):
        self.simulation = simulation
        self.states = states
    
    def plot_evolution(self):
        variables = self.simulation.model.influence_graph.list_variables()
        t = np.arange(0, len(self.states), 1)
        fig, ax = plt.subplots()
        
        for variable in variables:
            ax.plot(t, [state[variable] for state in self.states])

        ax.set(xlabel='step', ylabel='state', title='Simulation')
        ax.grid()
        plt.show()
