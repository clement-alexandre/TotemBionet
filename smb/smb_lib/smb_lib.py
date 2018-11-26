from py4j.java_gateway import JavaGateway


gateway = JavaGateway()
smbionet = gateway.entry_point.getSmbionet();



class SmbionetException(Exception):
    def __init__(self, message=None, cause=None):
        self.message = message
        self.cause = cause
    
    def __str__(self):
        return self.message or str(self.cause)



def addGrapheInfluence(graphe):
    smbionet.generateInputFile(graphe)
    return graphe

def addCTL(ctl):
    smbionet.addCTL(ctl)
    return ctl

def runSMBionet():
    smbionet.run()
    return "SMBionet is running"

def resultSMBionet():
    return smbionet.result()
