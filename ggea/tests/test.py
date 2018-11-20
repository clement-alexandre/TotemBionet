# -*- coding: utf8 -*-

import unittest
import os

from ggea import GGEAModel, Graph, create_graph, export_to_dot

model2346 = GGEAModel()
model2346.variables = {"CTr": [0, 1], "CEc": [0, 1], "SG1": [0, 1, 2],
                       "SG2": [0, 1, 2], "Sc": [0, 1], "Ecel": [0, 1],
                       "EBGL": [0, 1], "I": [0, 1, 2]}
model2346.relations = {"CTr": {"K_CTr": 1, "K_CTr+u6_acti": 1},
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
model2346.multiplex = {
    'CTr': {'u6_acti': '(I < 2) and ((CTr == 1) and ((SG1 == 2) or (SG2 > '
                       '0)))'},
    'CEc': {'u7_acti': '(I < 1) and ((CEc == 1) and (SG1 > 0))'},
    'SG1': {'u1_inhi': '(CTr == 0) or ((SG1 < 2) and (SG2 == 0))',
            'u2_inhi': '(CEc == 0) and (SG1 == 0)',
            'u4_acti': '(SG2 == 0) and (EBGL == 1)'},
    'SG2': {'u5_acti': '((Sc == 1) and (Ecel == 1)) or ((SG2 < 2) or ('
                       'EBGL == 0))'}, 'Sc': {},
    'Ecel': {'u1_acti': '(CTr == 1) and ((SG1 == 2) or (SG2 > 0))'},
    'EBGL': {'u1_acti': '(CTr == 1) and ((SG1 == 2) or (SG2 > 0))'},
    'I': {'u2_acti': '(CEc == 1) and (SG1 > 0)'}}


class Test(unittest.TestCase):
    def test_empty_model(self):
        model = GGEAModel()
        graph = create_graph(model)
        self.assertEqual(0, graph.number_of_nodes())

    def test_ggea_example(self):
        graph = create_graph(model2346)
        self.assertEqual(864, graph.number_of_nodes())

    def test_export(self):
        expected = create_graph(model2346)
        export_to_dot("output", expected)
        from networkx.drawing.nx_pydot import read_dot
        actual = Graph(read_dot("output.dot"))
        self.assertEqual(expected, actual)
        os.remove("output.dot")

if __name__ == '__main__':
    unittest.main()
