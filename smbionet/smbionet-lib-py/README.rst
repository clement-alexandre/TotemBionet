Readme de smb-lib

librairie pipy : https://pypi.org/project/smb-lib/
librairie anaconda : https://anaconda.org/mohamedchennouf/smb-lib/


# How to Use this librairie:

import smb_lib

ip = "Ip of your API smbionet" #you can have in SMBIONET.md after running ./build.sh
smb_lib(ip).runSmbionet("graphe","ctl")
smb_lib(ip).consulteExperiences()
