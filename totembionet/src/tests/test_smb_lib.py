# coding: utf-8

import unittest
import os
import sys
import smb_lib

from .resources import mucus_operon_v1_out, mucus_operon_v1_smb, mucus_operon_v1_smv


class Test(unittest.TestCase):
    def _test_runSmbionet(self):
        smb_lib.smbionet().runSmbionet(mucus_operon_v1_smb)
        self.assertTrue(os.path.exists(mucus_operon_v1_smv))
        self.assertTrue(os.path.exists(mucus_operon_v1_out))
        os.remove(mucus_operon_v1_smv)
        os.remove(mucus_operon_v1_out)

    def _test_verify_modeles(self):
        smb = smb_lib.smbionet()
        smb.runSmbionet(mucus_operon_v1_smb)
        self.assertEqual(len(smb.getModeles()),42)
        os.remove(mucus_operon_v1_smv)
        os.remove(mucus_operon_v1_out)


if __name__ == '__main__':
    unittest.main()
