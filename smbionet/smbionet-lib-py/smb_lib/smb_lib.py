# -*- coding: utf8 -*-

from py4j.java_gateway import JavaGateway


gateway = JavaGateway()
smbionet = gateway.entry_point.getSmbionet();



class SmbionetException(Exception):
    def __init__(self, message=None, cause=None):
        self.message = message
        self.cause = cause
    
    def __str__(self):
        return self.message or str(self.cause)



def addGrapheInfluence(graphe,ctl):
    smbionet.generateInput(graphe,ctl)
    return "graphe :"+graphe +" ctl : "+ ctl


def generateInputwithPath(pathGraph , pathCTL):
    smbionet.generateInputwithPath(pathGraph,pathCTL)
    return "generation sucessfull"


def runSMBionet():
    smbionet.run()
    return "SMBionet is running"

def runSMBionetPathFile(path):
    smbionet.run(path)
    return "SMBionet is running"

def resultSMBionet():
    return smbionet.result()
