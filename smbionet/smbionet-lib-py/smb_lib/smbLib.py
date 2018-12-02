import requests


def runSmbionet(graph , ctl):
    input = graph + ctl;
    jsonInput = {"event": "GETALLMODELE", "experience" : { "id": "0", "input": input}}
    requestRunSMB = requests.post('http://localhost:9080/smbionet-service-document/modeles',json = jsonInput)
    print(requestRunSMB.text)
    return requestRunSMB.text



def consulteExperiences():
    requestConsults = requests.post('http://localhost:9080/smbionet-service-document/modeles',json = {"event": "CONSULT"})
    print(requestConsults.status_code)
    print(requestConsults.text)
    return requestConsults.text