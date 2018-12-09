# -*- coding: utf8 -*-

import unittest
import os
from collections import defaultdict

from ggea import Graph, create_graph, export_to_dot
from discrete_model import DiscreteModel, InfluenceGraph, parse_smbionet_output_file


class Test(unittest.TestCase):
    def test_empty_model(self):
        graph = create_graph(DiscreteModel(InfluenceGraph()))
        self.assertEqual(0, graph.number_of_nodes())

    def test_ggea_example(self):
        model2346, *_ = parse_smbionet_output_file('resources/model2346.out')
        graph = create_graph(model2346)
        self.assertEqual(864, graph.number_of_nodes())

    def test_export(self):
        model2346, *_ = parse_smbionet_output_file('resources/model2346.out')
        expected = create_graph(model2346)
        export_to_dot("output", expected)
        from networkx.drawing.nx_pydot import read_dot
        actual = Graph(read_dot("output.dot"))
        self.assertEqual(expected, actual)
        os.remove("output.dot")


if __name__ == '__main__':
    unittest.main()
