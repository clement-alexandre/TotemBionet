# -*- coding: utf8 -*-

import unittest
import os

from model_picker import pick_a_model_randomly, ModelPickerException


class Test(unittest.TestCase):
    def test_empty_model(self):
        self.assertRaises(ModelPickerException, pick_a_model_randomly, [])

    def test_radom_pick(self):
        models = [object(), object(), object()]
        model = pick_a_model_randomly(models)
        self.assertTrue(model in models)


if __name__ == '__main__':
    unittest.main()
