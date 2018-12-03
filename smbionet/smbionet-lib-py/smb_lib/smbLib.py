import requests


class smbionet:
    def __init__(self, ip):
        self.ip = ip

    def runSmbionet(self, graph , ctl, allModel):
        input = graph + ctl;
        jsonInput = {"event": "GETVALIDEMODELE", "experience" : { "id": "0", "input": input}}
        if allModel:
            jsonInput = {"event": "GETALLMODELE", "experience" : { "id": "0", "input": input}}
        requestRunSMB = requests.post('http://'+self.ip+':9080/smbionet-service-document/modeles',json = jsonInput)
        print(requestRunSMB.text)
        return requestRunSMB.text

    def runSmbionetwithPathFile(self,path,allModel):
        file = open(path, "r")
        input = file.read()
        jsonInput = {"event": "GETVALIDEMODELE", "experience" : { "id": "0", "input": input}}
        if allModel:
            jsonInput = {"event": "GETALLMODELE", "experience" : { "id": "0", "input": input}}
        requestRunSMB = requests.post('http://'+self.ip+':9080/smbionet-service-document/modeles',json = jsonInput)
        print(requestRunSMB.text)
        newFile = path[:-3] + "out";
        file = open(newFile,"w")
        file.write(requestRunSMB.text)
        file.close()
        return requestRunSMB.text

    def runSmbionetwithTwoPathFile(self, pathGraphe, pathCTL, allModel):
        fileGraphe = open(pathGraphe, "r")
        fileCTL = open(pathCTL, "r")
        input = fileGraphe.read() + fileCTL.read()
        jsonInput = {"event": "GETVALIDEMODELE", "experience" : { "id": "0", "input": input}}
        if allModel:
            jsonInput = {"event": "GETALLMODELE", "experience" : { "id": "0", "input": input}}
        requestRunSMB = requests.post('http://'+self.ip+':9080/smbionet-service-document/modeles',json = jsonInput)
        print(requestRunSMB.text)
        newFile = pathGraphe[:-3] + "out";
        file = open(newFile,"w")
        file.write(requestRunSMB.text)
        file.close()
        return requestRunSMB.text

    def purge(self):
        jsonInput = {"event": "PURGE"}
        requestRunSMB = requests.post('http://'+self.ip+':9080/smbionet-service-document/modeles',json = jsonInput)
        print(requestRunSMB.text)
        return requestRunSMB.text

    def consulteExperiences(self):
        requestConsults = requests.post('http://'+self.ip+':9080/smbionet-service-document/modeles',json = {"event": "CONSULT"})
        print(requestConsults.status_code)
        print(requestConsults.text)
        return requestConsults.text

    def getIp(self):
        return self.ip