package code;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class Out {

    private static String previous="";
    private static BufferedWriter w;
    private static  int verb;
    
    //Ecriture dans le fichier de sortie
    public static void pfln()throws IOException{w.write("\n");}    
    public static void pfln(String s)throws IOException{w.write(s+"\n");}    
    public static void pf(String s)throws IOException{w.write(s);}
    
    //Ecriture dans le shell et le fichier
    public static void pln()throws IOException{System.out.println();pfln();}    
    public static void pln(String s)throws IOException{System.out.println(s);pfln(s);}    
    public static void p(String s)throws IOException{System.out.print(s);pf(s);}

    //Ecriture que le shell
    public static void pnf(String s)throws IOException{System.out.print(s);}

    //Ecriture dans le shell avec retour du chariot

    public static void pr(){
	String white="";
	for(int i=0;i<previous.length();i++)
	    white+=" ";
	System.out.print("\r"+white+"\r");
	previous="";
    }
    
    public static void pr(String s){
	String white="";
	for(int i=0;i<previous.length();i++)
	    white+=" ";
	System.out.print("\r"+white+"\r"+s);
	previous=s;
    }

    //Gestion des variables
    
    public static void prln(String s){
	pr(s+"\n");
    }

    public static void setVerb(int v){
	verb=v;
    }

    public static int verb(){
	return verb;
    }

    public static void printIn(String file)throws IOException{
	w=new BufferedWriter(new FileWriter(new File(file)));
    }

    public static void close()throws IOException{w.close();}

    

}