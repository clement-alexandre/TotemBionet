# -*- coding: utf8 -*-

import random
from typing import Dict, List
from discrete_model import DiscreteModel, InfluenceGraph, Gene, Transition
import matplotlib.pyplot as plt
import numpy as np


class Simulation:
    def __init__(self, model: DiscreteModel):
        self.model = model
        self.random = random.Random()
        self.steps = 100
        self.initial_state = None

    def run(self) -> 'Result':
        states = [self._initial_state()]
        for _ in range(self.steps-1):
            states.append(self._next_step(states[-1]))
        return Result(self, states)

    def _initial_state(self):
        if self.initial_state:
            return {self.model.find_gene_by_name(gene): state
                    for gene, state in self.initial_state.items()}
        return {gene: self.random.choice(gene.states) for gene in self.model.genes}

    def _next_step(self, states: Dict[Gene, int]):
        return {gene: self._future(gene, states) for gene, state in states.items()}

    def _future(self, gene, states):
        available_states = self.model.available_state(gene, states)
        return self.random.choice(available_states)


class Result:
    def __init__(self, simulation: Simulation, states: List[Dict[Gene, int]]):
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
        plt.show()
