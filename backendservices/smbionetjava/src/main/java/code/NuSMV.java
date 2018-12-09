package code;

import java.io.*;

public class NuSMV{

	private static String NUSMVPATH="NuSMV";

	public static void setPath(String path){
		NUSMVPATH=path;
	}

	private static void writeNet(Net net,String file)throws Exception{
		BufferedWriter w = new BufferedWriter(new FileWriter(new File(file)));
		w.write("-- NuSMV file written by SMBioNet\n\nMODULE main\n\n");
		//VAR
		w.write("VAR\n\n");
		for(int i=0;i<net.size();i++){
			w.write(net.get(i)+" : "+net.get(i).min+" .. "+net.get(i).max+" ;\n");
		}
		//DEFINE
		w.write("\nDEFINE\n\n");
		//D�finition, pour chaque g�ne g, le fonctions qui donne le
		//param�tre vers lequel g �volue en fonction de l'�tat du
		//r�seau
		for(int i=0;i<net.size();i++){
			//Cette fonction s'appelle F_g
			w.write("F_"+net.get(i)+" :=");
			//Cas particulier ou le g�ne � 1 param�tre
			if(net.get(i).paras.size()==1)
				//On �crit juste la valeur de ce param�tre (valeur
				//quelconque prise dans l'intervalle courant)
				w.write(" "+net.get(i).paras.get(0).currentInt().min+" ; -- "+
						net.get(i).paras.get(0)+"\n\n");
				//Cas g�n�ral
			else{
				w.write("\ncase\n");
				//Pour chaque param�tre
				for(int j=net.get(i).paras.size()-1;j>=0;j--){
					Para p=net.get(i).paras.get(j);
					//on �crit les formules r�gulations associ�es � p
					if(p.regs.isEmpty())
						//Cas particulier o� il n'y a pas de formule
						w.write("TRUE");
					else
						//Cas g�n�ral
						for(int k=0;k<p.regs.size();k++){
							w.write(p.regs.get(k).formula.toString());
							if(k<p.regs.size()-1)
								w.write(" & ");
						}
					//On �crit la valeur acutelle du param�tre (valeur
					//quelconque prise dans l'intervalle courant)
					w.write(" : "+p.currentInt().min+" ; -- "+p+"\n");
				}
				w.write("esac;\n\n");
			}
		}
		//ASSIGN
		w.write("ASSIGN\n\n");
		//Directions d'�volution des g�nes
		for(int i=0;i<net.size();i++)
			//Pour la version NuSMV 2.4 qui fait la diff�rence entre
			//le type boole�n et entier
			if(net.get(i).max-net.get(i).min==1)
				w.write("next("+net.get(i)+") :=\ncase\n"+
						//Stabilit�
						net.get(i)+" =  F_"+net.get(i)+" : "+net.get(i)+" ;\n"+
						//Evolution
						" TRUE : {"+net.get(i).min+", "+net.get(i).max+"} ;\n"+
						"esac;\n\n");
			else
				w.write("next("+net.get(i)+") :=\ncase\n"+
						//Stabilit�
						net.get(i)+" = F_"+net.get(i)+" : "+net.get(i)+" ;\n"+
						//Augmentation
						net.get(i)+" < F_"+net.get(i)+" : {"+net.get(i)+", "+net.get(i)+" + 1} ;\n"+
						//Diminution
						net.get(i)+" > F_"+net.get(i)+" : {"+net.get(i)+" - 1, "+net.get(i)+"} ;\n"+
						"esac;\n\n");
		//TRANS
		w.write("TRANS\n\n");
		//Stabilit�
		w.write("(");
		for(int i=0;i<net.size();i++){
			w.write(net.get(i)+" = F_"+net.get(i));
			if(i<net.size()-1)
				w.write(" & ");
		}
		w.write(") |\n");
		//Ou �volution du i�me g�ne uniquement
		for(int i=0;i<net.size();i++){
			w.write("(");
			for(int j=0;j<net.size();j++){
				if(i==j)
					w.write(net.get(j)+" != next("+net.get(j)+")");
				else
					w.write(net.get(j)+"  = next("+net.get(j)+")");
				if(j<net.size()-1)
					w.write(" & ");
			}
			w.write(")");
			if(i<net.size()-1)
				w.write(" |\n");
		}
		//SPEC
		w.write("\n\nSPEC\n");
		if(net.ctl.equals(""))
			w.write("TRUE\n\n");
		else
			w.write(net.ctl+"\n\n");
		w.close();
	}

	public static boolean check(Net net,String file,boolean dynamics,boolean inversion)throws Exception{

		//�criture du fichier NuSMV
		writeNet(net,file);
		//EXECUTION DE NUSMV
		Process proc;
		if(dynamics) {

			//avec r�ordonnement dynamique des variables
			proc = Runtime.getRuntime().exec(NUSMVPATH + " -dynamic " + file);
		}
		else {

			//sans r�ordonnement dynamique des variables
			proc = Runtime.getRuntime().exec(NUSMVPATH + " " + file);
		}
		//LECTURE DU RESULTAT
		InputStream inputstream=proc.getInputStream();
		InputStreamReader inputstreamreader=new InputStreamReader(inputstream);
		BufferedReader reader=new BufferedReader(inputstreamreader);
		String line;
		//On passe les 26 premi�res lignes
		for(int i=0;i<16;i++)
			reader.readLine();
		//On enregistre la 27i�me ligne
		line=reader.readLine();
		//Si elle est non null, on peut donner le r�sultat
		if(line!=null){
			proc.destroy();
			if(!inversion)
				return line.endsWith("true");
			else
				return !line.endsWith("true");
		}
		//Sinon, il y a un probl�me d'execution et on affiche le
		//message d'erreur de NuSMV
		InputStream errstream=proc.getErrorStream();
		InputStreamReader errstreamreader=new InputStreamReader(errstream);
		reader=new BufferedReader(errstreamreader);
		String message="";
		while((line=reader.readLine())!=null)
			message+=line+"\n";
		if(message.equals(""))
			message="";
		throw new Exception(message);
	}

}
