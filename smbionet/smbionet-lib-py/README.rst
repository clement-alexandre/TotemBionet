Readme de smb-lib

librairie pipy : https://pypi.org/project/smb-lib/
librairie anaconda : https://anaconda.org/mohamedchennouf/smb-lib/


# How to Use this librairie:

import smb_lib

smb = smb_lib.smbionet("192.168.1.17")  # you can have ip in SMBIONET.md after running ./build.sh
smb.runSmbionet(graphe,ctl)
smb.consulteExperiences()
smb.purge()
...
