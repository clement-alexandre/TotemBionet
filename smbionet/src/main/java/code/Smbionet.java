package code;


import java.util.*;
import java.io.*;
import java.math.*;
import jclock.*;
import jlist.List;

public class Smbionet {
    private static Clock c;
    private static long nbParas;
    private static long nbGoodParas;
    private String input;
    private List<String> opts = new List<String>();
    private int indiceFile = 0;

    public Smbionet(){
    }

    //Option s avec une entier en argument

    public String generateInputFile(String grapheInfluence)  {
        try {
            input = "./samples/result"+indiceFile;
            Out.printIn(input+".txt");
            Out.pf(grapheInfluence);
            indiceFile++;
        }catch (Exception e){
            System.err.println("\n"+e.getMessage());
        }
        return grapheInfluence;
    }


    public void addCTL(String ctl){
        try {
            Out.pf(ctl);
            Out.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    public void run(){
        if(input != null) {
            try {
                //Fichier de sortie
                Out.printIn(getOpt("-o", input + ".out", opts));
                //Niveau d'�criture
                Out.setVerb(getOpt("-v", 0, opts));

                //CONSTRUCTION DU R�SEAU
                Net net = new Net(input+".txt");
                net.printoo();
                if (getOpt("-comp", opts))
                    System.exit(0);

                //SIMULATION
                if (getOpt("-simu", opts)) {
                    Simu.run(net);
                    Out.close();
                    System.exit(0);
                }

                //ENUMERATION/SELECTION
                //Options
                NuSMV.setPath(getOpt("-path", "NuSMV", opts));
                boolean dynamic = opts.contains("-dynamic");
                boolean inversion = opts.contains("-inversion");
                //Comptage
                nbParas = 0;
                nbGoodParas = 0;
                //Pour l'affichage toute les secondes
                c = new Clock();
                Timer timer = new Timer();
                timer.schedule(new Banner(), 0, 1000);
                //Premier param�trage
                net.firstParameterization();
                do {
                    nbParas++;
                    //S�lection
                    if (NuSMV.check(net, input + ".smv", dynamic, inversion)) {
                        //Ecriture du param�trage dans le fichier de sortie
                        nbGoodParas++;
                        Out.pf("# MODEL " + nbGoodParas);
                        if (Out.verb() > 0)
                            Out.pf(" (id = " + net.idCurrentParameterization() + ")");
                        Out.pfln("\n");
                        net.printCurrentParameterization();
                    } else {
                        Out.pf("# IMODEL");
                        Out.pfln("\n");
                        net.printCurrentParameterization();
                    }
                    //Changement de param�trage
                } while (net.nextParameterization());
                Out.pr();
                Out.pln("# SELECTED MODELS | CHECKED MODELS = " +
                        nbGoodParas + " / " + nbParas + " (" + c + ")");
                Out.close();
                //System.exit(0);

            } catch (InterruptedException e) {
                System.err.println("\n" + e.getMessage());
                //e.printStackTrace();
                System.exit(2);
            } catch (FileNotFoundException e) {
                System.err.println("\n" + e.getMessage());
                //e.printStackTrace();
                System.exit(2);
            } catch (Exception e) {
                System.err.println("\n" + e.getMessage());
                //e.printStackTrace();
                System.exit(2);
            }
        }else{
            System.out.println("pas input");
        }
    }

    public String result(){
        if(input != null) {
            return Out.readFile(input + ".out");
        }else{
            return "Error pas de fichier:"+input;
        }
    }

    private int getOpt(String s,int def,List<String> opts)throws Exception{
        int i=opts.indexOf(s);
        if(i>=0){
            if(i+1<opts.size())
                return Integer.valueOf(opts.get(i+1));
            throw new Exception("option "+s+" needs an int as arg");
        }
        return def;
    }

    //Option avec une chaine de caract�res en argument

    private String getOpt(String s,String def,List<String> opts)throws Exception{
        int i=opts.indexOf(s);
        if(i>=0){
            if(i+1<opts.size() && !opts.get(i+1).startsWith("-"))
                return opts.get(i+1);
            throw new Exception("option "+s+" needs a string as arg");
        }
        return def;
    }

    //Pr�sence d'une option

    private boolean getOpt(String s,List<String> opts){
        return opts.contains(s);
    }

    public void helloword(){
        System.out.println("ok momo");
    }

    //Pour l'affichage toutes les secondes

    private class Banner extends TimerTask{

        public void run(){
            Out.pr("> SELECTED MODELS / CHECKED MODELS = "+
                    nbGoodParas+" / "+nbParas+" ("+c+")");
        }
    }

    // Getters and Setters

    public String getInput() {
        return input;
    }

    public void setInput(String input) {
        this.input = input;
    }

    public List<String> getOpts() {
        return opts;
    }

    public void setOpts(List<String> opts) {
        this.opts = opts;
    }

    public int getIndiceFile() {
        return indiceFile;
    }

    public void setIndiceFile(int indiceFile) {
        this.indiceFile = indiceFile;
    }

}
