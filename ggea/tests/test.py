# -*- coding: utf8 -*-

import unittest
import os
from collections import defaultdict

from ggea import GGEAModel, Graph, parse, create_graph, export_to_dot


variables = {"CTr": [0, 1], "CEc": [0, 1], "SG1": [0, 2],
                       "SG2": [0, 2], "Sc": [0, 1], "Ecel": [0, 1],
                       "EBGL": [0, 1], "I": [0, 2]}
relations = {"CTr": {"K_CTr": 1, "K_CTr+u6_acti": 1},
                       "CEc": {"K_CEc": 1, "K_CEc+u7_acti": 1},
                       "SG1": {"K_SG1": 0, "K_SG1+u1_inhi": 0,
                               "K_SG1+u2_inhi": 2, "K_SG1+u4_acti": 2,
                               "K_SG1+u1_inhi+u2_inhi": 2,
                               "K_SG1+u2_inhi+u4_acti": 2,
                               "K_SG1+u1_inhi+u4_acti": 2,
                               "K_SG1+u1_inhi+u2_inhi+u4_acti": 2},
                       "SG2": {"K_SG2": 0, "K_SG2+u5_acti": 0},
                       "Sc": {"K_Sc": 1},
                       "Ecel": {"K_Ecel": 0, "K_Ecel+u1_acti": 1},
                       "EBGL": {"K_EBGL": 1, "K_EBGL+u1_acti": 1},
                       "I": {"K_I": 0, "K_I+u2_acti": 2}}
multiplex = {
    'CTr': {'u6_acti': '(I < 2) and ((CTr == 1) and ((SG1 == 2) or (SG2 > '
                       '0)))'},
    'CEc': {'u7_acti': '(I < 1) and ((CEc == 1) and (SG1 > 0))'},
    'SG1': {'u1_inhi': '(CTr == 0) or ((SG1 < 2) and (SG2 == 0))',
            'u2_inhi': '(CEc == 0) and (SG1 == 0)',
            'u4_acti': '(SG2 == 2) and (EBGL == 1)'},
    'SG2': {'u5_acti': '((Sc == 1) and (Ecel == 1)) or ((SG2 < 2) or ('
                       'EBGL == 0))'},
    'Ecel': {'u1_acti': '(CTr == 1) and ((SG1 == 2) or (SG2 > 0))'},
    'EBGL': {'u1_acti': '(CTr == 1) and ((SG1 == 2) or (SG2 > 0))'},
    'I': {'u2_acti': '(CEc == 1) and (SG1 > 0)'}}
model2346 = GGEAModel(variables, relations, multiplex)


class Test(unittest.TestCase):
    def test_parsing(self):
        self.maxDiff = None
        with open('resources/model2346.out') as file:
            parsed_models = parse(file.read())
            self.assertTrue(any(model2346 == model for model in parsed_models))

    def test_empty_model(self):
        model = GGEAModel()
        graph = create_graph(model)
        self.assertEqual(0, graph.number_of_nodes())

    def test_ggea_example(self):
        graph = create_graph(model2346)
        self.assertEqual(279, graph.number_of_nodes())

    def test_export(self):
        expected = create_graph(model2346)
        export_to_dot("output", expected)
        from networkx.drawing.nx_pydot import read_dot
        actual = Graph(read_dot("output.dot"))
        self.assertEqual(expected, actual)
        os.remove("output.dot")

if __name__ == '__main__':
    unittest.main()
