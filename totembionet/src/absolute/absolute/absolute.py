import os


class abs:
    def __init__(self):
        self.modeles = []

    def runAbs(self,path):
        if os.path.exists("/notebook/requierements/HolmesBionet/solver.opt"):
            os.system("chmod 777 /notebook/requierements/HolmesBionet/solver.opt")
            cmd = "./solver.opt "+path;
            os.system(cmd)
            so = os.popen(cmd).read()
            print(so)  
        else:
            print("fail")


    def runHolmes(self,path):
            os.system("chmod 777 /notebook/requierements/HolmesBionet/modelling.sh")
            cmd = "/notebook/requierements/HolmesBionet/modelling.sh -i /notebook/requierements/HolmesBionet/examples/path_test -n 100 -t 48"
            result = os.system(cmd)
            print(result)