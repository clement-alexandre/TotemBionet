# coding: utf-8

import unittest

import pandas

from discrete_model import (Gene, Multiplex, InfluenceGraph, DiscreteModel, 
                            Expression, State, Transition, parse_smbionet_output_file,
                            ResourceTable, ResourceTableWithModel, export_model_to_json,
                            parse_json_model)


class Test(unittest.TestCase):
    def test_process_activation(self):
        model1, _ = parse_smbionet_output_file('resources/mucusOperonV3.out')
        free = model1.find_multiplex_by_name('free')
        mucuB = model1.find_gene_by_name('mucuB')
        self.assertTrue(free.is_active(State({mucuB: 0})))

    def test_resources_table_as_data_frame(self):
        model1, *_ = parse_smbionet_output_file('resources/mucusOperonV4.out')
        rt = ResourceTable(model1.influence_graph)
        df = rt.as_data_frame()
        expected = pandas.DataFrame({
            'operon': [0, 0, 1, 1, 2, 2],
            'mucuB': [0, 1, 0, 1, 0, 1],
            'active multiplex on operon': ['{free}', '{}', '{free}', '{}', '{free, alg}', '{alg}'],
            'active multiplex on mucuB': ['{}', '{}', '{prod}', '{prod}', '{prod}', '{prod}']
        })
        pandas.testing.assert_frame_equal(expected, df)

    def test_resources_table_as_data_frame_with_model(self):
        model1, *_ = parse_smbionet_output_file('resources/mucusOperonV4.out')
        rt = ResourceTableWithModel(model1)
        df = rt.as_data_frame()
        expected = pandas.DataFrame({
            'operon': [0, 0, 1, 1, 2, 2],
            'mucuB': [0, 1, 0, 1, 0, 1],
            'active multiplex on operon': ['{free}', '{}', '{free}', '{}', '{free, alg}', '{alg}'],
            'active multiplex on mucuB': ['{}', '{}', '{prod}', '{prod}', '{prod}', '{prod}'],
            'K_operon': ['2', '0', '2', '0', '2', '2'],
            'K_mucuB': ['0', '0', '1', '1', '1', '1']
        })
        pandas.testing.assert_frame_equal(expected, df)

    def test_find_transition(self):
        model1, model2 = parse_smbionet_output_file('resources/mucusOperonV3.out')
        operon = model1.find_gene_by_name('operon')
        free = model1.find_multiplex_by_name('free')
        alg = model1.find_multiplex_by_name('alg')
        transition = model1.find_transition(operon, (alg, free))
        self.assertEqual((2,), transition.states)
    
        transition = model2.find_transition(operon, (free,))
        self.assertEqual((1, 2), transition.states)
    
    def test_available_states(self):
        _, model2 = parse_smbionet_output_file('resources/mucusOperonV3.out')
        operon = model2.find_gene_by_name('operon')
        mucuB = model2.find_gene_by_name('mucuB')
        self.assertEqual((State({operon: 1, mucuB: 0}),), model2.available_state_for_gene(operon, State({operon: 0, mucuB: 0})))
        self.assertEqual((State({operon: 2, mucuB: 0}),), model2.available_state_for_gene(operon, State({operon: 1, mucuB: 0})))
        self.assertEqual((State({operon: 2, mucuB: 0}),), model2.available_state_for_gene(operon, State({operon: 2, mucuB: 0})))

    def test_cycling_states(self):
        model, *_ = parse_smbionet_output_file('resources/mucusOperonV4.out')
        operon = model.find_gene_by_name('operon')
        mucuB = model.find_gene_by_name('mucuB')
        self.assertEqual((State({operon: 1, mucuB: 0}),), model.available_state(State({operon: 0, mucuB: 0})))
        self.assertEqual((State({operon: 2, mucuB: 0}), State({operon: 1, mucuB: 1})), model.available_state(State({operon: 1, mucuB: 0})))
        self.assertEqual((State({operon: 0, mucuB: 1}),), model.available_state(State({operon: 1, mucuB: 1})))
        self.assertEqual((State({operon: 0, mucuB: 0}),), model.available_state(State({operon: 0, mucuB: 1})))

    def test_mucus_operon_v3(self):
        model1, model2 = parse_smbionet_output_file('resources/mucusOperonV3.out')
        graph = InfluenceGraph()
        operon = Gene('operon', (0, 1, 2))
        graph.add_gene(operon)
        mucuB = Gene('mucuB', (0, 1))
        graph.add_gene(mucuB)
        free = Multiplex('free', (operon,), Expression('(not(mucuB>=1))'))
        graph.add_multiplex(free)
        alg = Multiplex('alg', (operon,), Expression('(operon>=1)'))
        graph.add_multiplex(alg)
        prod = Multiplex('prod', (mucuB,), Expression('(operon>=1)'))
        graph.add_multiplex(prod)

        expected_model1 = DiscreteModel(graph)
        expected_model1.add_transition(Transition(operon, (), (0,)))
        expected_model1.add_transition(Transition(operon, (alg,), (2,)))
        expected_model1.add_transition(Transition(operon, (free,), (0,)))
        expected_model1.add_transition(Transition(operon, (alg, free), (2,)))
        expected_model1.add_transition(Transition(mucuB, (), (0,)))
        expected_model1.add_transition(Transition(mucuB, (prod,), (1,)))

        expected_model2 = DiscreteModel(graph)
        expected_model2.add_transition(Transition(operon, (), (0,)))
        expected_model2.add_transition(Transition(operon, (alg,), (2,)))
        expected_model2.add_transition(Transition(operon, (free,), (1, 2)))
        expected_model2.add_transition(Transition(operon, (alg, free), (2,)))
        expected_model2.add_transition(Transition(mucuB, (), (0,)))
        expected_model2.add_transition(Transition(mucuB, (prod,), (1,)))

        self.assertEqual(expected_model1, model1)
        self.assertEqual(expected_model2, model2)
    
    def test_export_model(self):
        model, *_ = parse_smbionet_output_file('resources/mucusOperonV4.out')
        json = export_model_to_json(model)
        self.assertEqual(model, parse_json_model(json))


if __name__ == '__main__':
    unittest.main()
