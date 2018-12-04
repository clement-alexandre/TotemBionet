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
    private String input = "result";
    private List<String> opts = new List<String>();

    public Smbionet(){
    }


    public void generateInput(String entre)  {
        try {
            Out.printIn(input+".txt");
            Out.pf(entre);
            Out.close();
        }catch (Exception e){
            System.err.println("\n"+e.getMessage());
        }
    }


    public void run(boolean allModel){
        if(input != null) {
            try {
                //Fichier de sortie
                Out.printIn(getOpt("-o", input + ".out", opts));
                System.out.println("ficher ok");
                //Niveau d'�criture
                Out.setVerb(getOpt("-v", 0, opts));
                System.out.println("niveau ecriture ok");

                //CONSTRUCTION DU R�SEAU
                Net net = new Net(input+".txt");
                net.printoo();


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
                    boolean t = NuSMV.check(net, input + ".smv", dynamic, inversion);
                    if (NuSMV.check(net, input + ".smv", dynamic, inversion)) {
                        //Ecriture du param�trage dans le fichier de sortie
                        nbGoodParas++;
                        Out.pf("# MODEL " + nbGoodParas);
                        if (Out.verb() > 0)
                            Out.pf(" (id = " + net.idCurrentParameterization() + ")");
                        Out.pfln("\n");
                        net.printCurrentParameterization();
                    } else {
                        if(allModel) {
                            Out.pf("# IMODEL");
                            Out.pfln("\n");
                            net.printCurrentParameterization();
                        }
                    }
                    //Changement de param�trage
                } while (net.nextParameterization());
                Out.pr();
                Out.pln("# SELECTED MODELS | CHECKED MODELS = " +
                        nbGoodParas + " / " + nbParas + " (" + c + ")");
                Out.close();
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


    public String readResult(){
        if(input != null) {
            return Out.readFile(input + ".out");
        }else{
            return "Error pas de fichier omg:"+input;
        }
    }

    public String readInput(){
        if(input != null) {
            return Out.readFile(input + ".txt");
        }else{
            return "Error bro:"+input;
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

}
