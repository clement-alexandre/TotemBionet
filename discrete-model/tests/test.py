import unittest
from discrete_model import parse_smbionet_output_file, InfluenceGraph, DiscreteModel
import pandas


class Test(unittest.TestCase):
    def test_process_activation(self):
        model1, _ = parse_smbionet_output_file('resources/mucusOperonV3.out')
        free = model1.find_process_by_name('free')
        mucuB = model1.find_gene_by_name('mucuB')
        self.assertTrue(free.is_active({mucuB: 0}))

    def test_resources_table(self):
        model1, *_ = parse_smbionet_output_file('resources/mucusOperonV4.out')
        operon = model1.find_gene_by_name('operon')
        mucuB = model1.find_gene_by_name('mucuB')
        free = model1.find_process_by_name('free')
        alg = model1.find_process_by_name('alg')
        prod = model1.find_process_by_name('prod')
        rt = model1.influence_graph.resources_table()
        self.assertEqual({prod}, rt.get_resources({operon: 1, mucuB: 1}))
        self.assertEqual({alg, free, prod}, rt.get_resources({operon: 2, mucuB: 0}))
        self.assertEqual({alg}, rt.get_resources_for_gene(operon, {operon: 2, mucuB: 1}))
        self.assertEqual("""operon | mucuB || active multiplex on operon | active multiplex on mucuB
------------------------------------------------------------------------
     0 |     0 || {free}                     | {}                       
     0 |     1 || {}                         | {}                       
     1 |     0 || {free}                     | {prod}                   
     1 |     1 || {}                         | {prod}                   
     2 |     0 || {free, alg}                | {prod}                   
     2 |     1 || {alg}                      | {prod}                   
""", str(rt))

    def test_resources_table_with_model(self):
        model1, *_ = parse_smbionet_output_file('resources/mucusOperonV4.out')
        rt = model1.influence_graph.resources_table()
        rt.model = model1
        self.assertEqual("""operon | mucuB || active multiplex on operon | active multiplex on mucuB || K_operon | K_mucuB
----------------------------------------------------------------------------------------------
     0 |     0 || {free}                     | {}                        || 1        | 0      
     0 |     1 || {}                         | {}                        || 0        | 0      
     1 |     0 || {free}                     | {prod}                    || 2        | 1      
     1 |     1 || {}                         | {prod}                    || 0        | 1      
     2 |     0 || {free, alg}                | {prod}                    || 2        | 1      
     2 |     1 || {alg}                      | {prod}                    || 2        | 1      
""", str(rt))

    def test_resources_table_as_data_frame_with_model(self):
        model1, *_ = parse_smbionet_output_file('resources/mucusOperonV4.out')
        rt = model1.resources_table()
        df = rt.as_data_frame()
        expected = pandas.DataFrame({
            'operon': [0, 0, 1, 1, 2, 2],
            'mucuB': [0, 1, 0, 1, 0, 1],
            'active multiplex on operon': ['{free}', '{}', '{free}', '{}', '{free, alg}', '{alg}'],
            'active multiplex on mucuB': ['{}', '{}', '{prod}', '{prod}', '{prod}', '{prod}'],
            'K_operon': ['1', '0', '2', '0', '2', '2'],
            'K_mucuB': ['0', '0', '1', '1', '1', '1']
        })
        pandas.testing.assert_frame_equal(expected, df)

    def test_find_transition(self):
        model1, model2 = parse_smbionet_output_file('resources/mucusOperonV3.out')
        operon = model1.find_gene_by_name('operon')
        free = model1.find_process_by_name('free')
        alg = model1.find_process_by_name('alg')
        transition = model1.find_transition(operon, (alg, free))
        self.assertEqual((2,), transition.states)
    
        transition = model2.find_transition(operon, (free,))
        self.assertEqual((1, 2), transition.states)
    
    def test_available_states(self):
        _, model2 = parse_smbionet_output_file('resources/mucusOperonV3.out')
        operon = model2.find_gene_by_name('operon')
        mucuB = model2.find_gene_by_name('mucuB')
        self.assertEqual((1,), model2.available_state(operon, {operon: 0, mucuB: 0}))
        self.assertEqual((2,), model2.available_state(operon, {operon: 1, mucuB: 0}))
        self.assertEqual((2,), model2.available_state(operon, {operon: 2, mucuB: 0}))

    def test_mucus_operon_v3(self):
        model1, model2 = parse_smbionet_output_file('resources/mucusOperonV3.out')
        graph = InfluenceGraph()
        graph.add_gene('operon', 0, 2)
        graph.add_gene('mucuB', 0, 1)
        graph.add_process('free', '(not(mucuB>=1))', 'operon')
        graph.add_process('alg', '(operon>=1)', 'operon')
        graph.add_process('prod', '(operon>=1)', 'mucuB')

        expected_model1 = DiscreteModel(graph)
        expected_model1.add_transition('operon', (), (0,))
        expected_model1.add_transition('operon', ('alg',), (2,))
        expected_model1.add_transition('operon', ('free',), (0,))
        expected_model1.add_transition('operon', ('alg', 'free'), (2,))
        expected_model1.add_transition('mucuB', (), (0,))
        expected_model1.add_transition('mucuB', ('prod',), (1,))

        expected_model2 = DiscreteModel(graph)
        expected_model2.add_transition('operon', (), (0,))
        expected_model2.add_transition('operon', ('alg',), (2,))
        expected_model2.add_transition('operon', ('free',), (1, 2))
        expected_model2.add_transition('operon' , ('alg', 'free'), (2,))
        expected_model2.add_transition('mucuB', (), (0,))
        expected_model2.add_transition('mucuB', ('prod',), (1,))

        self.assertEqual(expected_model1, model1)
        self.assertEqual(expected_model2, model2)


if __name__ == '__main__':
    unittest.main()
