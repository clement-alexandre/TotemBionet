import requests


class smbionet:
    def __init__(self, ip):
        self.ip = ip

    def runSmbionet(self, graph , ctl):
        input = graph + ctl;
        jsonInput = {"event": "GETALLMODELE", "experience" : { "id": "0", "input": input}}
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


