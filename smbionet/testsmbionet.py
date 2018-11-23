from py4j.java_gateway import JavaGateway

gateway = JavaGateway()
smbionet = gateway.entry_point.getSmbionet();





grapheInfluence = "# exemple mucus graphe figure 6\n" \
                  "# haut page 21 : pas d'information sur les paramètres mais formule CTL\n" \
                  "\n" \
                  "VAR\n" \
                  "operon = 0 2;\n" \
                  "mucuB = 0 1;\n" \
                  "\n" \
                  "REG\n" \
                  "prod [(operon>=1)] => mucuB;\n" \
                  "free [!(mucuB>=1)] => operon;\n" \
                  "alg [(operon>=1)] => operon;\n" \
                  "\n" \
                  "PARA\n" \
                  "K_operon = 0 ;\n" \
                  "K_operon+alg = 2 ;\n" \
                  "# K_operon+free = 2 ;\n" \
                  "K_operon+alg+free = 2 ;\n" \
                  "K_mucuB = 0 ;\n" \
                  "K_mucuB+prod = 1 ;\n";

smbionet.generateInputFile(grapheInfluence);

ctl =  "\n" \
       "CTL\n" \
       "(operon=0)->AG(!(mucuB=1)) & (operon=2)->AG(!(mucuB=0))";

smbionet.addCTL(ctl)

smbionet.run();

