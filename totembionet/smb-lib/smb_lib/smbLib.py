import os


class smbionet:
    def __init__(self,message=None):
        self.message = message

    def runSmbionet(self,path):
        os.system('java -cp ../smbionetjava.jar code.Main '+path)
        lookup = 'MODEL'
        lookupTwo = 'IMODEL'
        find = False
        checked = 0
        totalchecked = 0
        with open(path[:-4]+".out") as myFile:
            lines = myFile.readlines()
            for line in lines[:-1]:
                if lookup in line:
                    checked += 1
                    totalchecked += 1
                    find = True
                if lookupTwo in line:
                    totalchecked += 1
                    find = False
                if(find):
                    print(line,end='')
        print("checkedModeles/totalModeles = "+str(checked)+"/"+str(totalchecked))


    def runSmbionetWithTwoPath(self, pathGraphe, pathCTL):
        fileGraphe = open(pathGraphe, "r")
        fileCTL = open(pathCTL, "r")
        inp = fileGraphe.read() +"\n\n"+ fileCTL.read()
        file = open("resources/result.smb","w")
        file.write(inp)
        file.close()
        os.system('java -cp ../smbionetjava.jar code.Main resources/result.smb')
        lookup = 'MODEL'
        lookupTwo = 'IMODEL'
        find = False
        checked = 0
        totalchecked = 0
        with open("resources/result.out") as myFile:
            lines = myFile.readlines()
            for line in lines[:-1]:
                if lookup in line:
                    checked += 1
                    totalchecked += 1
                    find = True
                if lookupTwo in line:
                    totalchecked += 1
                    find = False
                if(find):
                    print(line,end='')
            print("checkedModeles/totalModeles = "+str(checked)+"/"+str(totalchecked))
        os.remove("resources/result.smb")

