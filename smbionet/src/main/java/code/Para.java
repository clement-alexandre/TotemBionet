package code;

import jlist.List;

public class Para {

    //Nom du param�tre
    public final String name;
    
    //Liste des r�gulations du param�tre
    public final List<Reg> regs;

    //Intervalles possibles pour le param�tres
    public final List<Interval> intervals=new List<Interval>();
    
    //Indice du plus petit et du plus grand intervalle que le
    //param�tre peut prendre
    private int minInt;
    private int maxInt;

    //Indice de l'interval courant associ� au param�tre
    private int currentInt;

    //Indice de l'interval maximum courant associ� au param�tre
    private int currentMaxInt;

    //Niveau maximum est minimum pour le param�tre <donn�s par
    //l'utilisateur>
    private int minLevel;
    private int maxLevel;
        
    //Param�tres dont l'ensemble des regulations contient
    //"imm�diatement" l'ensemble des regulations de regs
    public final List<Para> succs=new List<Para>();

    //Constructeur
    
    Para(String name, List<Reg> regs){
	this.name=name;
	this.regs=regs;
    }

    //La description = le nom
    
    public String toString(){
	return name;
    }
    
    //�criture des intervalles possibles
    
    protected String intervalsToString(){
	String s="";
	for(int i=0;i<minInt;i++)
	    s+=intervals.get(i).toString();
	s+=" {";
	for(int i=minInt;i<=maxInt;i++)
	    s+=intervals.get(i).toString();
	s+="} ";
	for(int i=maxInt+1;i<intervals.size();i++)
	    s+=intervals.get(i).toString();	
	return s;
    }

    //CONSULTATIONS SUR LES INTERVALLES ET VALEURS POSSIBLES

    //Donne le plus grand intervalle possible

     public Interval maxInt(){
 	return intervals.get(maxInt);
     }

    //Donne le plus petit intervalle possible
    
    public Interval minInt(){
	return intervals.get(minInt);
    }

    //Donne l'intervalle courant

    public Interval currentInt(){
 	return intervals.get(currentInt);
    }
    
    //Donne le niveau minimal (indiqu� par l'utilisateur)
    
    public int minLevel(){
	return minLevel;
    }

    //Donne le niveau maximal (indiqu� par l'utilisateur)
    
    public int maxLevel(){
	return maxLevel;
    }
    
    //MODIFICATION DES INTERVALLES MIN ET MAX ET DES NIVEAUX MIN ET MAX

    //Apr�s cette m�thode: le plus petit (grand) interval possible est
    //celui contenant min (max)

    public boolean setMinMax(int minLevel,int maxLevel){
	int minInt=-1;
	int maxInt=-1;
	for(int i=0;i<intervals.size();i++){
	    if(intervals.get(i).min<=minLevel && minLevel<=intervals.get(i).max)
		minInt=i;
	    if(intervals.get(i).min<=maxLevel && maxLevel<=intervals.get(i).max)
		maxInt=i;
	}
	//Si tout va bien, on change effectivement minInt, LastInt,
	//minLevel et maxLevel
	if(minLevel<=maxLevel && minInt>=0 && maxInt>=0){
	    this.minLevel=minLevel;
	    this.maxLevel=maxLevel;
	    this.minInt=minInt;
	    this.maxInt=maxInt;
	    return true;
	}
	return false;
    }

    //Re-initialise minInt et maxInt en fonction de minLevel et maxLevel

    public void updateMinIntMaxInt(){
	setMinMax(minLevel,maxLevel);
    }
    
    //R�duit l'intervalle max jusqu'� ce qu'il soit <=, au sens
    //faible, � l'intervalle transmis en argument
    
    public boolean setMaxIntWeakLessThan(Interval interval){
	while(!maxInt().weakLess(interval) && maxInt>=minInt)
	    maxInt--;
	return maxInt().weakLess(interval);
    }

    //Augmente l'intervalle min jusqu'� ce qu'il soit >=, au sens
    //faible, � l'intervalle transmis en argument

    public boolean setMinIntWeakGreaterThan(Interval interval){
	while(!interval.weakLess(minInt()) && minInt<=maxInt)
	    minInt++;
	return interval.weakLess(minInt());
    }

    //MODIFICATION DE L'INTERVALLE COURANT ET DE L'INTERVALLE MAX COURANT

    //L'intervalle courant est le plus petit possible

    public void firstCurrentInt(){
	currentInt=minInt;
    }

    //Augmentation de l'intervalle courant, s'il ateind l'intervalle
    //max courant on le met au min et on retourne faux, sionn on
    //retourne true

    public boolean nextCurrentInt(){
	if(currentInt<currentMaxInt){
	    currentInt++;
	    return true;
	}
	currentInt=minInt;
	return false;
    }

    //Apr�s l'appel, l'intervalle max courant est l'intervalle max

    public void setCurrentMaxToMax(){
	currentMaxInt=maxInt;
    }
    
    //Apr�s l'appel � cette m�thode, l'intervalle max courant est le
    //plus grand intervalle <=, au sens faible, � l'intervalle courant
    //des successeurs du param�tre

    public void setCurrentMaxLessThanSuccs(){
	//On regarde la plus petite borne sup de l'intervalle courant
	//des successeurs
	int smallMax=intervals.get(maxInt).max;
	for(int i=0;i<succs.size();i++)
	    if(succs.get(i).currentInt().max<smallMax)
		smallMax=succs.get(i).currentInt().max;
	//L'intervalle max courant et celui qui contient cette plus
	//petite borne sup
	for(int i=0;i<intervals.size();i++)
	    if(intervals.get(i).min<=smallMax && smallMax<=intervals.get(i).max){
		currentMaxInt=i;
		break;
	    }	
    }

}