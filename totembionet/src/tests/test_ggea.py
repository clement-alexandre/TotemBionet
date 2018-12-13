# coding: utf-8

import unittest
import os

from ggea import Graph
from discrete_model import DiscreteModel, InfluenceGraph, parse_smbionet_output_file

from .resources import resources, model2346_out


class Test(unittest.TestCase):
    def test_empty_model(self):
        graph = Graph(DiscreteModel(InfluenceGraph()))
        self.assertEqual(0, graph.number_of_states())

    def test_ggea_example(self):
        model, *_ = parse_smbionet_output_file(model2346_out)
        graph = Graph(model)
        self.assertEqual(864, graph.number_of_states())

    def test_export(self):
        model, *_ = parse_smbionet_output_file(model2346_out)
        graph = Graph(model)
        output = os.path.join(resources, 'output')
        graph.export_to_dot(output)
        self.assertTrue(os.path.exists(output + '.dot'))
        os.remove(output + '.dot')


if __name__ == '__main__':
    unittest.main()
