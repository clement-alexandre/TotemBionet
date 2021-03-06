# coding: utf-8

import unittest

from discrete_model import parse_smbionet_output_file, InfluenceGraph, DiscreteModel
from simu_net import Simulation, Result

from .resources import day_night_cycle_out, model2346_out


class Test(unittest.TestCase):
    def test_model_2346(self):
        model1, *_ = parse_smbionet_output_file(model2346_out)
        simulation = Simulation(model1)
        result = simulation.run()
        self.assertEqual(100, len(result.states))

    def test_day_night_cycle_out(self):
        model1, *_ = parse_smbionet_output_file(day_night_cycle_out)
        simulation = Simulation(model1)
        simulation.steps = 5
        simulation.random.seed(0xff)
        simulation.initial_state = {'G': 0, 'P': 1}
        result = simulation.run()
        G = model1.find_gene_by_name('G')
        P = model1.find_gene_by_name('P')
        expected = [
            {G: 0, P: 1},
            {G: 0, P: 0},
            {G: 1, P: 0},
            {G: 1, P: 1},
            {G: 0, P: 1}
        ]
        self.assertEqual(expected, result.states)


if __name__ == '__main__':
    unittest.main()
