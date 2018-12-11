# coding: utf8

import random
from typing import Dict, List
from discrete_model import DiscreteModel, InfluenceGraph, Gene, Transition, State
import matplotlib.pyplot as plt
import numpy as np


class Simulation:
    def __init__(self, model: DiscreteModel):
        self.model = model
        self.random = random.Random()
        self.steps = 100
        self.initial_state = {gene.name: self.random.choice(gene.states) 
                              for gene in self.model.genes}

    def run(self) -> 'Result':
        states = [self._convert_initial_state_to_state()]
        for _ in range(self.steps-1):
            states.append(self._next_step(states[-1]))
        return Result(self, states)

    def _convert_initial_state_to_state(self) -> State:
        return State({self.model.find_gene_by_name(gene): state
                      for gene, state in self.initial_state.items()})

    def _next_step(self, state: State):
        next_states = self.model.available_state(state)
        if next_states:
            return self.random.choice(next_states)
        return state


class Result:
    def __init__(self, simulation: Simulation, states: List[State]):
        self.simulation = simulation
        self.states = states

    def plot_evolution(self):
        genes = self.simulation.model.genes
        t = np.arange(0, len(self.states), 1)
        _, ax = plt.subplots()
        for gene in genes:
            ax.plot(t, [state[gene] for state in self.states], label=gene.name)
        ax.set(xlabel='step', ylabel='state', title='Simulation')
        ax.grid()
        plt.legend()
        plt.show()
