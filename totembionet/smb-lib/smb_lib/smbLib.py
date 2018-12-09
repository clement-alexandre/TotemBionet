import requests, json


class smbionet:
    def __init__(self,message=None):
        self.message = message


    def runSmbionet(self, graph , ctl, allModel):
        input = graph + ctl;
        jsonInput = {"event": "GETVALIDEMODELE", "experience" : {"input": input}}
        if allModel:
            jsonInput = {"event": "GETALLMODELE", "experience" : {"input": input}}
        requestRunSMB = requests.post('http://localhost:9080/smbionet-service-document/modeles',json = jsonInput)
        resultJson = json.loads(requestRunSMB.text)
        output = resultJson["experience"]["output"]
        newFile = "./../resources/result.out";
        file = open(newFile,"w")
        file.write(output)
        file.close()
        return output

    def runSmbionetwithPathFile(self,path,allModel):
        file = open(path, "r")
        input = file.read()
        jsonInput = {"event": "GETVALIDEMODELE", "experience" : {"input": input}}
        if allModel:
            jsonInput = {"event": "GETALLMODELE", "experience" : {"input": input}}
        requestRunSMB = requests.post('http://localhost:9080/smbionet-service-document/modeles',json = jsonInput)
        resultJson = json.loads(requestRunSMB.text)
        output = resultJson["experience"]["output"]
        newFile = path[:-3] + "out";
        file = open(newFile,"w")
        file.write(output)
        file.close()
        return output

    def runSmbionetwithTwoPathFile(self, pathGraphe, pathCTL, allModel):
        fileGraphe = open(pathGraphe, "r")
        fileCTL = open(pathCTL, "r")
        input = fileGraphe.read() + fileCTL.read()
        jsonInput = {"event": "GETVALIDEMODELE", "experience" : {"input": input}}
        if allModel:
            jsonInput = {"event": "GETALLMODELE", "experience" : {"input": input}}
        requestRunSMB = requests.post('http://localhost:9080/smbionet-service-document/modeles',json = jsonInput)
        resultJson = json.loads(requestRunSMB.text)
        output = resultJson["experience"]["output"]
        newFile = pathGraphe[:-3] + "out";
        file = open(newFile,"w")
        file.write(output)
        file.close()
        return output

    def purge(self):
        jsonInput = {"event": "PURGE"}
        requestRunSMB = requests.post('http://localhost:9080/smbionet-service-document/modeles',json = jsonInput)
        return requestRunSMB.text

    def consulteExperiences(self):
        requestConsults = requests.post('http://localhost:9080/smbionet-service-document/modeles',json = {"event": "CONSULT"})
        return requestConsults.text