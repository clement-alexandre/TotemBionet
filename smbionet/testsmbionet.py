from py4j.java_gateway import JavaGateway

gateway = JavaGateway()
smbionet = gateway.entry_point.getSmbionet();


smbionet.run("/Users/mohamedchennouf/Desktop/TotemBionet/smbionet/samples/mucusOperonV3");

