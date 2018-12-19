# coding: utf-8

import unittest

import pandas

from discrete_model import parse_smbionet_output_file
from resource_table import ResourceTable, ResourceTableWithModel

from .resources import mucus_operon_v3_out, mucus_operon_v4_out


class Test(unittest.TestCase):
    def test_resources_table_as_data_frame(self):
        model1, *_ = parse_smbionet_output_file(mucus_operon_v4_out)
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
        model1, *_ = parse_smbionet_output_file(mucus_operon_v4_out)
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


if __name__ == '__main__':
    unittest.main()
