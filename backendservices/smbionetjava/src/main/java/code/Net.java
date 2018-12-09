package code;

import java.math.*;
import java.io.*;
import jlist.*;
import jclock.*;

public class Net extends List<Gene>{

	public String ctl;

	//CONSTRUCTION DU RESEAU

	public Net(String inputFile)throws Exception{

		//Lecture des g�nes
		Reader.readVar(inputFile,this);
		//Lecture des r�gulations
		Reader.readReg(inputFile,this);
		//Construction du r�seau
		build();
		//Calcul des param�tres
		for(int i=0;i<size();i++)
			get(i).setParas();
		//Lecture des contraintes sur les param�tres
		Reader.readPara(inputFile,this);
		//Construction des dags
		for(int i=0;i<size();i++)
			get(i).setDag();
		//Lecture de la formule CTL
		ctl=Reader.readCTL(inputFile);
	}

	//(1) Pour chaque g�ne, on construit la liste des g�nes qui
	//interviennent dans ses r�gulations. (2) On remplace les
	//variables qui interviennent dans la formule de chaque r�gulation
	//par les g�nes du r�seau.

	private void build()throws Exception{
		//Pr�sence de g�nes
		if(isEmpty())
			throw new Exception("No genes in the net");
		//On v�rifie qu'il n'y a pas des g�nes diff�rents avec le m�me nom
		if(hasRedundancy())
			throw new Exception("There are several genes with the same name");
		//POUR CHAQUE GENE
		for(int i=0;i<size();i++){
			//On v�rifie les bornes
			if(get(i).min<0 || get(i).max<get(i).min)
				throw new Exception("Bad bounds for gene "+get(i));
			//On v�rifie qu'il n'y a pas des r�gulations de m�me nom
			if(get(i).regs.hasRedundancy())
				throw new Exception("Gene "+get(i)+" has several regulations with the same name");
			//On vide la liste des g�nes entrant
			get(i).inputGenes.clear();
			//POUR CHAQUE REGULATION
			for(int j=0;j<get(i).regs.size();j++)
				//On donne la couleur 1 � toutes les variables
				//pr�sentent dans la formule de la r�gulation
				get(i).regs.get(j).formula.setColor(1);
			//On donne la couleur 0 au g�ne
			get(i).setColor(0);
		}
		//CONSTRUCTION DES LIENS GENE -> GENE
		for(int i=0;i<size();i++){
			for(int j=0;j<get(i).regs.size();j++){
				//Insertions des g�nes du r�seau dans la formule de r
				for(int k=0;k<size();k++)
					//Si l'insertion du g�ne k a lieu, on l'ajoute
					//dans la liste des g�nes entrant du g�ne i
					if(get(i).regs.get(j).formula.insert(get(k)))
						if(!get(i).inputGenes.contains(get(k)))
							get(i).inputGenes.add(get(k));
				//Si toutes la variables pr�sentent dans la formule de
				//la r�gulation sont de couleur 0, c'est que toutes
				//les variables correspondent des g�nes
				if(!get(i).regs.get(j).formula.colorOfAllVarIs(0))
					throw new Exception("Unknown variables in regulation "+
							get(i).regs.get(j)+" of gene "+get(i));
			}
		}
	}

	//ENUMERATION DES PARAMETRAGES

	public void firstParameterization(){
		for(int i=0;i<size();i++)
			get(i).firstParameterization();
	}

	public boolean nextParameterization(){
		for(int i=0;i<size();i++)
			if(get(i).nextParameterization())
				return true;
		return false;
	}

	//AFFICHAGE

	public void printoo()throws IOException{
		//GENES
		Out.pln("\nVAR\n");
		BigInteger nbStates=BigInteger.ONE;
		for(int i=0;i<size();i++){
			Out.pln(get(i)+" = "+get(i).min+" "+get(i).max+" ;");
			nbStates=nbStates.multiply(BigInteger.valueOf(get(i).max-get(i).min+1));
		}
		if(Out.verb()>0)
			Out.pln("\n# Number of states = "+nbStates);
		//REGULATIONS
		//Calcul de la liste de r�gulation
		List<Reg> regs=new List<Reg>();
		for(int i=0;i<size();i++)
			for(int j=0;j<get(i).regs.size();j++)
				if(!regs.contains(get(i).regs.get(j)))
					regs.add(get(i).regs.get(j));
		Out.pln("\nREG\n");
		for(int i=0;i<regs.size();i++){
			Out.p(regs.get(i)+" ["+regs.get(i).formula+"]=>");
			for(int j=0;j<size();j++)
				if(get(j).regs.contains(regs.get(i)))
					Out.p(" "+get(j));
			Out.pln(" ;");
		}
		//PARAMETRES
		Out.pln("\nPARA\n");
		BigInteger nb=BigInteger.ONE;
		Clock c1=new Clock();
		for(int i=0;i<size();i++){
			//Affichage des param�tres
			Out.pln("# Parameters for "+get(i)+"\n");
			for(int j=0;j<get(i).paras.size();j++){
				Para p=get(i).paras.get(j);
				Out.p(p+" = "+p.minLevel()+" ");
				if(p.minLevel()<p.maxLevel())
					Out.p(p.maxLevel()+" ");
				Out.p(";");
				if(Out.verb()>2)
					Out.p("   # "+p.intervalsToString());
				Out.pln();
			}
			Out.pln();
			//dag
			if(Out.verb()>1)
				get(i).printDag();
			if(Out.verb()>0){
				//Nombre de param�trages
				Clock c2=new Clock();
				Out.p("# number of parameterizations =");
				BigInteger tmp=get(i).nbParameterizations();
				Out.pln(" "+tmp+" ("+c2+")\n");
				nb=nb.multiply(tmp);
			}
		}
		//FORMULE CTL
		if(ctl!=null && !ctl.equals(""))
			Out.pln("CTL\n"+ctl+"\n");
		else
			Out.pln("# NO CTL FORMULA\n");
		if(Out.verb()>0)
			Out.pln("# Total number of parameterizations = "+nb+" ("+c1+")\n");
	}

	//Affichage du param�trage courant

	public void printCurrentParameterization()throws IOException{
		for(int i=0;i<size();i++){
			for(int j=0;j<get(i).paras.size();j++){
				Out.pf("# "+get(i).paras.get(j)+" = "+get(i).paras.get(j).currentInt().min);
				if(get(i).paras.get(j).currentInt().min<get(i).paras.get(j).currentInt().max)
					Out.pf(" "+get(i).paras.get(j).currentInt().max);
				Out.pfln();
			}
			Out.pfln();
		}
	}

	//Code un indice pour la param�trage courant qui ne d�pend que du
	//r�seau.

	public BigInteger idCurrentParameterization(){
		BigInteger base=BigInteger.ONE;
		BigInteger id=BigInteger.ZERO;
		BigInteger min,max,level;
		for(int i=0;i<size();i++){
			for(int j=0;j<get(i).paras.size();j++){
				min=BigInteger.valueOf((long)get(i).min);
				max=BigInteger.valueOf((long)get(i).max);
				level=BigInteger.valueOf((long)get(i).paras.get(j).currentInt().min);
				id=id.add((level.subtract(min)).multiply(base));
				base=base.multiply((max.subtract(min)).add(BigInteger.ONE));
			}
		}
		return id;
	}

}