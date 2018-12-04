import requests, json


class save:
    def __init__(self, ip):
        self.ip = ip

    def saveExperience(self, exp, id):
        jsonInput = {"event": "SAVE", "experience" : {"experience": exp, "id":id}}
        requestSave = requests.post('http://'+self.ip+':9090/save-service-document/save',json = jsonInput)
        resultJson = json.loads(requestSave.text)
        return resultJson



    def getExperience(self,id):
        jsonInput = {"event": "GET", "id" : id}
        requestSave = requests.post('http://'+self.ip+':9090/save-service-document/save',json = jsonInput)
        resultJson = json.loads(requestSave.text)
        return resultJson


    def downloadExperience(self,id,pathDownload):
        jsonInput = {"event": "GET", "id" : id}
        requestSave = requests.post('http://'+self.ip+':9090/save-service-document/save',json = jsonInput)
        resultJson = json.loads(requestSave.text)
        output = resultJson["experience"]
        newFile = pathDownload + "result.out";
        file = open(newFile,"w")
        file.write(output)
        file.close()
        return output


    def getAllExperience(self):
        jsonInput = {"event": "GETALL"}
        requestSave = requests.post('http://'+self+':9090/save-service-document/save',json = jsonInput)
        resultJson = json.loads(requestSave.text)
        return resultJson

    def purge(self):
        jsonInput = {"event": "PURGE"}
        requestRunSMB = requests.post('http://'+self.ip+':9080/smbionet-service-document/modeles',json = jsonInput)
        return requestRunSMB.text

    def getIp(self):
        return self.ip