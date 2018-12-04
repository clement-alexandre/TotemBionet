package code;

import java.util.*;
import java.io.*;
import java.math.*;
import jclock.*;
import jlist.List;


class Main{

    private static Clock c;
    private static long nbParas;
    private static long nbGoodParas;

    public static void main(String args[]){
	try{
	    
	    //GESTION DES ARGUMENTS
	    //Pr�sence du fichier d'entr�e
	    if(args.length==0)
		throw new Exception("No input file");
	    //Fichiers entr�e
	    String input=args[args.length-1];
	    
	    //OPTIONS
	    List<String> opts=new List<String>();
	    for(int i=0;i<args.length-1;i++)
		opts.add(args[i]);
	    //Fichier de sortie
	    Out.printIn(getOpt("-o",input+".out",opts));
	    //Niveau d'�criture
	    Out.setVerb(getOpt("-v",0,opts));
	    
	    //CONSTRUCTION DU R�SEAU
	    Net net=new Net(input);
	    net.printoo();
	    if(getOpt("-comp",opts))
		System.exit(0);
	    	    
	    //SIMULATION 
	    if(getOpt("-simu",opts)){
		Simu.run(net);
		Out.close();
		System.exit(0);
	    }
	    
	    //ENUMERATION/SELECTION
	    //Options
	    NuSMV.setPath(getOpt("-path","NuSMV",opts));
	    boolean dynamic=opts.contains("-dynamic");
	    boolean inversion=opts.contains("-inversion");
	    //Comptage
	    nbParas=0;
 	    nbGoodParas=0;
	    //Pour l'affichage toute les secondes
	    c=new Clock();
	    Timer timer=new Timer();
 	    timer.schedule(new Banner(),0,1000);
	    //Premier param�trage
	    net.firstParameterization();
	    do{
 		nbParas++;
 		//S�lection
 		if(NuSMV.check(net,input+".smv",dynamic,inversion)){
		    //Ecriture du param�trage dans le fichier de sortie
 		    nbGoodParas++;
 		    Out.pf("# MODEL "+nbGoodParas);
		    if(Out.verb()>0)
			Out.pf(" (id = "+net.idCurrentParameterization()+")");
 		    Out.pfln("\n");
 		    net.printCurrentParameterization();
 		}
 		else
 		{
 			Out.pf("# IMODEL");
		    Out.pfln("\n");
 		    net.printCurrentParameterization();
 		}
		//Changement de param�trage
	    }while(net.nextParameterization());
 	    Out.pr();
 	    Out.pln("# SELECTED MODELS | CHECKED MODELS = "+
		    nbGoodParas+" / "+nbParas+" ("+c+")");
	    Out.close();
	    System.exit(0);
	    
	}catch(InterruptedException e){
	    System.err.println("\n"+e.getMessage());
	    //e.printStackTrace();
	    System.exit(2);
	}catch(FileNotFoundException e){
	    System.err.println("\n"+e.getMessage());
	    //e.printStackTrace();
	    System.exit(2);
	}catch(Exception e){
	    System.err.println("\n"+e.getMessage());
	    //e.printStackTrace();
	    System.exit(2);
	}
    }

    //Option s avec une entier en argument

    private static int getOpt(String s,int def,List<String> opts)throws Exception{
	int i=opts.indexOf(s);
	if(i>=0){
	    if(i+1<opts.size())
		return Integer.valueOf(opts.get(i+1));
	    throw new Exception("option "+s+" needs an int as arg");
	}
	return def;
    }
    
    //Option avec une chaine de caract�res en argument

    private static String getOpt(String s,String def,List<String> opts)throws Exception{
 	int i=opts.indexOf(s);
 	if(i>=0){
 	    if(i+1<opts.size() && !opts.get(i+1).startsWith("-"))
 		return opts.get(i+1);
 	    throw new Exception("option "+s+" needs a string as arg");
 	}
 	return def;
    }

    //Pr�sence d'une option

    private static boolean getOpt(String s,List<String> opts){
	return opts.contains(s);
    }

    //Pour l'affichage toutes les secondes

    private static class Banner extends TimerTask{
	
	public void run(){
	    Out.pr("> SELECTED MODELS / CHECKED MODELS = "+
		   nbGoodParas+" / "+nbParas+" ("+c+")");
	}
    }

}