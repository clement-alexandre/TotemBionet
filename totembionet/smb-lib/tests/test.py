# -*- coding: utf8 -*-

import unittest
import os
import smb_lib


class Test(unittest.TestCase):
    def test_runSmbionet(self):
        smb_lib.smbionet().runSmbionet("./../tutorials/resources/mucusOperonV1.smb")
        self.assertTrue(os.path.exists("./../tutorials/resources/mucusOperonV1.smv"))
        self.assertTrue(os.path.exists("./../tutorials/resources/mucusOperonV1.out"))

    def test_verify_modeles(self):
        smb = smb_lib.smbionet()
        smb.runSmbionet("./../tutorials/resources/mucusOperonV1.smb")
        self.assertEqual(len(smb.getModeles()),42)

if __name__ == '__main__':
    unittest.main()
