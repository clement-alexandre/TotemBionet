package code;

import py4j.GatewayServer;

public class SmbionetEntryPoint {

    private Smbionet smbionet;

    public SmbionetEntryPoint() {
        smbionet = new Smbionet();
    }

    public Smbionet getSmbionet() {
        return smbionet;
    }

    public static void main(String[] args) {
        GatewayServer gatewayServer = new GatewayServer(new SmbionetEntryPoint());
        gatewayServer.start();
        System.out.println("Gateway Server Started");
    }

}
