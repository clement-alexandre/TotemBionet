package code;

import jlist.List;

import java.io.BufferedReader;
import java.io.InputStreamReader;

public class Simu {

    //Les tendances
    private static final int UP=1;
    private static final int STEADY=0;
    private static final int DOWN=-1;
    private static final int UNKNOWN=-2;
    
    //Lecture des commandes
    private static BufferedReader in=new BufferedReader(new InputStreamReader(System.in));
    
    //Niveaux des g�nes � l'�tat courant
    private static int level[];
    //Param�tres focaux � l'�tat courant
    private static Para para[];
    //Niveau max des param�tres focaux
    private static int max[];
    //Niveau min des param�tres focaux;
    private static int min[];
    //Tendance des g�nes � l'�tat courant
    private static int tendancy[];        
    //Pour la commande goto
    private static final List<int[]> states=new List<int[]>();
    //Pour la commande print
    private static final List<String> simu=new List<String>();
    
    //Parcours interactif dans le graphe de transitions d'�tats
    
    public static void run(Net net){
	try{
	    //Initialisation des variables
	    level=new int[net.size()];
	    para=new Para[net.size()];
	    max=new int[net.size()];
	    min=new int[net.size()];
	    tendancy=new int[net.size()];
	    //�tat initial
	    Out.pln("# SIMULATION MODE");
	    Out.pnf("> init\n");
	    init(net);
	    //Addition de l'�tat et mise � jour des variables
	    keepState(net);
	    while(true){
		Out.pnf("> ");
		//Lecture de la commade
		String command=removeWhite(in.readLine());
		//Commande vide
		if(command.equals(""))
		    continue;
		//Exit
		if(command.equals("quit"))
		    break;
		//Help
		if(command.equals("help")){
		    Out.pnf("> use: name_of_gene, index_of_state, init, print, quit, help\n");
		    continue;
		}
		//Evolution d'un g�ne
		int g=net.indexOf(net.get(command));
		if(g>=0){
		    //Augmentation du g�ne
		    if(tendancy[g]==UP){
			level[g]+=1;
			keepState(net);
			continue;
		    }
		    //Diminution du g�ne
		    if(tendancy[g]==DOWN){
			level[g]-=1;
			keepState(net);
			continue;
		    }
		    //Pas de changement d'�tat
		    if(tendancy[g]==STEADY)
			Out.pnf("> gene "+command+" is steady\n");
		    if(tendancy[g]==UNKNOWN)
			Out.pnf("> unknown tendancy\n");
		    continue;
		}
		//Retour � un �tat pr�c�dant
		try{
		    //Indice de l'�tat
		    int t=Integer.valueOf(command);
		    if(t<0 || t>=states.size()){
			Out.pnf("> bad index of state\n");
			continue;
		    }
		    //Enregistrement l'�tat t
		    for(int i=0;i<net.size();i++)
			level[i]=states.get(t)[i];
		    //Suppression de la m�moire � partir de t (t compris)
		    while(states.size()>t){
			states.remove(t);
			simu.remove(t);
		    }
		    //On revient � l'�tat t
		    keepState(net);
		    continue;
		}catch(NumberFormatException e){}
		//Re-initialisation
		if(command.equals("init")){
		    init(net);
		    keepState(net);
		    continue;
		}
		//Affichage de la simu
		if(command.equals("print")){
		    for(int i=0;i<simu.size();i++)
			Out.p(simu.get(i));
		    continue;
		}
		//Mauvaise commande
		Out.pnf("> not a command (see commands with help)\n");
	    }
	}catch(Exception e){
	    System.err.println("Error : "+e);
	    e.printStackTrace();
	    System.exit(2);
	}
    }

    //R�-initialisation interactive

    private static void init(Net net)throws Exception{
	for(int i=0;i<net.size();i++){
	    while(true){
		Out.pnf("> "+net.get(i)+" = ");
		try{
		    level[i]=Integer.valueOf(removeWhite(in.readLine()));
		}catch(NumberFormatException e){
		    Out.pnf("> bad level for "+net.get(i)+"\n");
		    continue;
		}
		if(level[i]<net.get(i).min || net.get(i).max<level[i]){
		    Out.pnf("> bad level for "+net.get(i)+"\n");
		    continue;
		}
		break;
	    }
	}
	states.clear();
	simu.clear();
    }

    //Enregistrement et affichage de l'�tat courant
        
    private static void keepState(Net net)throws Exception{
	//Mise � jour de l'�tat du r�seau par level et m�morisation
	int mem[]=new int[net.size()];
	for(int i=0;i<net.size();i++){
	    mem[i]=level[i];
	    net.get(i).setLevel(level[i]);
	}
	states.add(mem);
	//Mise � jour des variables
	boolean end=true;
	for(int i=0;i<net.size();i++){
	    para[i]=net.get(i).getFocalPara();
	    min[i]=para[i].minInt().min;
	    max[i]=para[i].maxInt().max;
	    tendancy[i]=UNKNOWN;
	    if(level[i]<min[i]){
		tendancy[i]=UP;
		end=false;
	    }
	    if(level[i]>max[i]){
		tendancy[i]=DOWN;
		end=false;
	    }
	    if(level[i]==min[i] && level[i]==max[i])
		tendancy[i]=STEADY;
	}
	//Affichage de l'�tat et enregistrement dans simu
	String message="";
	if(states.size()==1)
	    message+=pMem("# SIMU\n");
	message+=pMem("# STATE "+(states.size()-1)+"\n");
	for(int i=0;i<net.size();i++){
	    //Niveau du g�ne
	    message+=pMem("# "+net.get(i)+" = "+level[i]+" ");
	    //Tendance du g�ne
	    if(tendancy[i]==UP)
		message+=pMem("[+]");
	    if(tendancy[i]==STEADY)
		message+=pMem("   ");
	    if(tendancy[i]==DOWN)
		message+=pMem("[-]");
	    if(tendancy[i]==UNKNOWN)
		message+=pMem("[?]");		  
	    //Param�tre
	    message+=pMem("  "+para[i]+" = "+min[i]);
	    if(max[i]>min[i])
		message+=pMem(" "+max[i]);
	    message+=pMem("\n");
	}
	if(end)
	    message+=pMem("# END\n");
	simu.add(message);
    }

    private static String pMem(String s)throws Exception{
	Out.p(s);
	return s;
    }

    //Suprime les characteres espaces devant et derrieres
    
    private static String removeWhite(String s)throws Exception{
	while(s.length()>0)
	    if(s.charAt(0)==' ' | s.charAt(0)=='\t')
		if(s.length()>1)
		    s=s.substring(1,s.length());
		else
		    s="";
	    else
		break;
	while(s.length()>0)
	    if(s.charAt(s.length()-1)==' ' | s.charAt(s.length()-1)=='\t')
		if(s.length()>1)
		    s=s.substring(0,s.length()-1);
		else
		    s="";
	    else
		break;
	return s;
    }



}