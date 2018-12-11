import os

class modele:
    def __init__(self, id, formule):
        self.id = id
        self.formule = formule

    def getId(self):
        return self.id

    def getFormule(self):
        return self.formule

class smbionet:
    def __init__(self):
        self.modeles = []

    def runSmbionet(self,path):
        if os.path.exists("/notebook/tutorials/requierements/smbionetjava.jar"):
            os.system('java -cp /notebook/tutorials/requierements/smbionetjava.jar code.Main '+path)
        else:
            print("le fichier smbionetjava n'exite pas")
        lookup = 'MODEL'
        lookupTwo = 'IMODEL'
        find = False
        checked = 0
        totalchecked = 0
        value = ""
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
                    if (lookup in line) == False:
                        value += line
                    else:
                        if(checked > 1):

                            self.modeles.append({"id":checked -1,"value":value.replace('\n', ' ')})
                            value =""
        print("checkedModeles/totalModeles = "+str(checked)+"/"+str(totalchecked))
        self.modeles.append({"id":checked,"value":value.replace('\n', ' ')})


    def getModeles(self):
        return self.modeles

    def printModeles(self,modeles):
        self.modeles = modeles
        for modele in self.modeles:
            print(modele.id)
            print(modele.formule)

    def runSmbionetWithTwoPath(self, pathGraphe, pathCTL):
        fileGraphe = open(pathGraphe, "r")
        fileCTL = open(pathCTL, "r")
        inp = fileGraphe.read() +"\n\n"+ fileCTL.read()
        path = pathGraphe[:-4]+"concat.smb"
        file = open(path,"w")
        file.write(inp)
        file.close()
        os.system('java -cp /notebook/tutorials/requierements/smbionetjava.jar code.Main '+path)
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
        os.remove(path)

