package code;

import jlist.List;
import jlogic.Var;

import java.io.IOException;
import java.math.BigInteger;

public class Gene extends Var {

    //Niveau min et max du gène
    public final int min;
    public final int max;
    //Régulations du gène
    public final List<Reg> regs=new List<Reg>();
    //Liste dans gènes intervenant dans les régulations
    public final List<Gene> inputGenes=new List<Gene>();
    //Paramètres associés au gène
    public final List<Para> paras=new List<Para>();

    //Pour l'énumération des paramètrages
    private List<List<Para>> dagByDeep=new List<List<Para>>();
    private int currentDeep;

    public Gene(String name, int min, int max){
	//Appel au constructeur de Var dans jlogic
	super(name);
	this.min=min;
	this.max=max;
    }

    //Premier niveau possible pour le gène

    private void firstLevel(){
	setLevel(min);//héritage
    }

    //Niveau suivant pour le gène s'il existe (true);
    //premier niveau sinon (false)

    private boolean nextLevel(){
	if(getLevel()==max){//héritage
	    setLevel(min);//héritage
	    return false;
	}
	setLevel(getLevel()+1);//héritage
	return true;
    }

    //Première configuration des gènes entrant

    private void firstConfig(){
	for(int i=0;i<inputGenes.size();i++)
	    inputGenes.get(i).firstLevel();
    }

    //Configuration suivante des gènes entrant

    private boolean nextConfig(){
	for(int i=0;i<inputGenes.size();i++)
	    if(inputGenes.get(i).nextLevel())
		return true;
	return false;
    }

    //retourne le paramètre vers lequel le gène évolue

    public Para getFocalPara(){
	for(int i=paras.size()-1;i>=0;i--){
	    Para p=paras.get(i);
	    for(int j=0;j<p.regs.size();j++)
		if(!p.regs.get(j).isEffective()){
		    p=null;
		    break;
		}
	    if(p!=null)
		return p;
	}
	return null;
    }

    //Construction de la liste de paramètres: (1) On construit les
    //paramètres possibles; (2) On calcul les intervalles possibles de
    //chaque paramètre; (3) On donne, par défaut, le plus grand
    //domaine de variation possible pour les paramètres; (4) On tri
    //les paramètres par ordre alphabétique et nombre de régulations
    //croissant

    public void setParas(){
	//On regarde si le gene s'autorégule
	boolean selfRegulated=inputGenes.contains(this);
	paras.clear();
	//Enumeration des configurations possibles pour les gènes entrant
	firstConfig();
	do{
	    //On fait la liste des régulations présentes
	    List<Reg> paraRegs=new List<Reg>();
	    for(int i=0;i<regs.size();i++)
		if(regs.get(i).isEffective())
		    paraRegs.addAlpha(regs.get(i));
	    //On construit le nom du paramètre
	    String paraName="K_"+name;
	    for(int i=0;i<paraRegs.size();i++)
		paraName+="+"+paraRegs.get(i);
	    //On regarde si le paramètre existe déjà
	    Para p=paras.get(paraName);
	    if(p==null){
		//Création du paramètre
		p=new Para(paraName,paraRegs);
		//Ajout dans la liste des paramètres
		paras.addAlpha(p);
		//Si le gène ne s'autorégule pas, tous les niveaux du
		//gène sont compatible avec le paramètre
		if(!selfRegulated)
		    for(int i=min;i<=max;i++)
			p.intervals.add(new Interval(i,true));
	    }
	    //Dans le cas où le gène s'autorégule, création d'un
	    //intervalle singleton = le paramètre est compatible avec
	    //le niveau actuelle du gène
	    if(selfRegulated){
		int position;
		for(position=0;position<p.intervals.size();position++)
		    if(getLevel()<=p.intervals.get(position).min){
			if(getLevel()==p.intervals.get(position).min)
			    position=-1;
			break;
		    }
		if(position>=0)
		    p.intervals.add(position,new Interval(getLevel(),true));
	    }
	}while(nextConfig());
	//Si le gène s'autorégule, on complète les intervalles
	//possibles pour chaque paramètre = les intervalles compris
	//entre les niveaux du gène compatible avec le paramètre
	if(selfRegulated)
	    for(int i=0;i<paras.size();i++){
		List<Interval> intervals=paras.get(i).intervals;
		//Intervalles intermédiaires
		for(int j=0;j<intervals.size()-1;j++)
		    if(intervals.get(j).max+1<=intervals.get(j+1).min-1){
			intervals.add(j+1,new Interval(intervals.get(j).max+1,intervals.get(j+1).min-1));
			j++;
		    }
		//Plus petit intervalle
		if(min<intervals.get(0).min)
		    intervals.add(0,new Interval(min,intervals.get(0).min-1));
		//Plus grand intervalle
		if(intervals.get(intervals.size()-1).max<max)
		    intervals.add(intervals.size(),
				  new Interval(intervals.get(intervals.size()-1).max+1,max));
	    }
	//On donne une valeur minimal et maximal pour chaque paramètre
	for(int i=0;i<paras.size();i++)
	    paras.get(i).setMinMax(min,max);
	//On tri les paramètres: si le nombre de régulateurs pour le
	//paramètre p est < au nombre de régulateurs pour p', alors p
	//est devant p' dans la liste
	boolean permut=true;
	while(permut){
	    permut=false;
	    for(int i=0;i<paras.size();i++)
		for(int j=i+1;j<paras.size();j++)
		    if(paras.get(j).regs.size()<paras.get(i).regs.size()){
			//permutation
			Para p=paras.get(i);
			paras.set(i,paras.get(j));
			paras.set(j,p);
			permut=true;
		    }
	}
    }

    //On construit le graphe d'inclusion entre les paramètres: (1)
    //C'est le plus petit graphe, tel qui si l'ensemble des
    //régulations de p est inclus dans l'ensemble des régulations de
    //p', alors il y a un chemin de p à p' dans le graphe; (2) S'il y
    //a un arc de p à p', alors la condition de monotonicité de p->p'
    //peut être violée; (3) Pour chaque arc p->p' on supprime les
    //intervalles de p et de p' qui rendent impossible la condition de
    //monotonicité

    public void setDag()throws Exception{
	//On initialise les arcs entrant et sortant et le domaine de
	//variation des paramètres (qui sera éventuellement réduit
	//lors de la construction du DAG)
	for(int i=0;i<paras.size();i++){
	    paras.get(i).succs.clear();
	    paras.get(i).updateMinIntMaxInt();
	}
	//Calcul des arcs (en profitant du fait que la liste des
	//paramètres soit triée par nombre de régulateurs croissant)
	for(int i=0;i<paras.size();i++){
	    Para p1=paras.get(i);
	    for(int j=i+1;j<paras.size();j++){
		Para p2=paras.get(j);
		if(p1.regs.sublist(p2.regs)){
		    //p2 est candidat pour être un successeur de p1,
		    //on regarde s'il est minimum pour l'inclusion
		    boolean arc=true;
		    for(int k=i+1;k<paras.size();k++)
			if(p1.regs.sublist(paras.get(k).regs) &&
			   paras.get(k).regs.strictSublist(p2.regs)){
			    arc=false;
			    break;
			}
		    //Avant de créer l'arc, on regarde en plus si la
		    //condition de monotonicité p1->p2 peut être fausse
		    if(arc && !p1.maxInt().weakLess(p2.minInt())){
			//Création d'un arc de p1 vers p2
			p1.succs.add(p2);
			//Si la condition de monotonicité ne peut
			//jamais être vérifiée, on a une exception
			if(!p1.minInt().weakLess(p2.maxInt()))
			    throw new Exception("No possible parameterization for gene "+this+
						"(parameter "+p1+" cannot be less than "+p2+")");
			//Réduction de l'intervalle max de p1
			p1.setMaxIntWeakLessThan(p2.maxInt());
			//Augmentation de l'intervalle min de p2
			p2.setMinIntWeakGreaterThan(p1.minInt());
		    }
		}
	    }
	}
	//Construction du "DAG par profondeur"
	setDagByDeep(0);
    }

    //Construction du "DAG par profondeur". C'est une liste de liste:
    //la liste 0 contient tous les paramètres sans successeur, et la
    //liste p>0 contient tous les paramètres dont tous les successeurs
    //sont dans la liste p-1;

    private void setDagByDeep(int deep){
	//Profondeur 0 = cas de base = tous les paramètres sans successeur
	if(deep==0){
	    List<Para> l=new List<Para>();
	    for(int i=0;i<paras.size();i++)
		if(paras.get(i).succs.isEmpty())
		    l.add(paras.get(i));
	    dagByDeep.clear();
	    dagByDeep.add(l);
	    setDagByDeep(1);
	}
	//Profondeur p>0 = tous les paramètres dont tous les
	//successeurs sont à la profondeur p-1
	else{
	    List<Para> l=new List<Para>();
	    for(int i=0;i<paras.size();i++)
		if(!paras.get(i).succs.isEmpty() &&
		   paras.get(i).succs.sublist(dagByDeep.get(deep-1)))
		    l.add(paras.get(i));
	    dagByDeep.add(l);
	    if(l.size()>0)
		setDagByDeep(deep+1);
	}
    }

    public void printDag()throws IOException{
	if(dagByDeep.size()>2){
	    Out.pln("# constraints:");
	    for(int i=0;i<paras.size();i++)
		for(int j=0;j<paras.get(i).succs.size();j++)
		    Out.pln("# "+paras.get(i)+" <= "+paras.get(i).succs.get(j));
	}
    }

    //ENUMERATION DES PARAMÉTRAGES

    public void firstParameterization(){
	//On minimise les intervalles courants de tous les paramètres
 	for(int i=0;i<paras.size();i++)
 	    paras.get(i).firstCurrentInt();
	//On maximise les intervalles max courants de tous les paramètres
	for(int i=0;i<paras.size();i++)
 	    paras.get(i).setCurrentMaxLessThanSuccs();
	//On part en bas du DAG
	currentDeep=dagByDeep.size()-1;
    }

    private boolean nextLocalParameterization(){
	//On augmente l'intervalle courant d'un paramètre à la profondeur currentDeep
 	for(int i=0;i<dagByDeep.get(currentDeep).size();i++){
	    //Si une augmentation a lieu, on augmente currentDeep
	    //d'une unité et on maximise l'intervalle max courant des
	    //paramètres à la nouvelle profondeur
 	    if(dagByDeep.get(currentDeep).get(i).nextCurrentInt()){
 		currentDeep++;
		for(int j=0;j<dagByDeep.get(currentDeep).size();j++)
		    dagByDeep.get(currentDeep).get(j).setCurrentMaxLessThanSuccs();
		return true;
 	    }
 	}
	//Si toutes les configurations ont été épuisées à la
	//profondeur currentDeep, on diminue la profondeur
	currentDeep--;
	return false;
    }

    public boolean nextParameterization(){
	while(currentDeep>=0)
	    if(nextLocalParameterization())
		return true;
	firstParameterization();
	return false;
    }

    //Nombre de paramétrages (monotones et sans redondance)

    public BigInteger nbParameterizations(){
	BigInteger nb=BigInteger.ONE;
	firstParameterization();
	while(nextParameterization())
	    nb=nb.add(BigInteger.ONE);
	return nb;
    }

}