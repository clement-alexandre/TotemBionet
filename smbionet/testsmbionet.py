from py4j.java_gateway import JavaGateway



gateway = JavaGateway()

smbionet = gateway.entry_point.getSmbionet();

smbionet.setInput("./samples/mucusOperonV2");
smbionet.run();

