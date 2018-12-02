package jlogic;

import java.util.StringTokenizer;

abstract public class Formula{
    
    //METHODES ABSTRAITES

    //�valuation de la formule
    abstract public int eval();
    //retourne une description de la formule
    abstract public String toString();
    //initialisation du niveau de la variable name
    abstract public void set(String name,int level);
    //insertion d'une variable dans la formule
    abstract public boolean insert(Var v);
    //coloration de toutes les variable avec la couleur c
    abstract public void setColor(int c);
    //indique si toutes les variables ont la couleur c
    abstract public boolean colorOfAllVarIs(int c);

    //SYNTAXE
    
    public static final String ALPHA="_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqurstuvwxyz";
    public static final String NUMBER="0123456789";
    
    //tous les termes qui suivent doivent utiliser des caract�res qui
    //ne sont ni dans ALPHA ni dans NUMBER
    
    private static final char OPEN='(';
    private static final char CLOSE=')';

    protected static final String EQUI="<->";
    protected static final String IMPLY="->";
    protected static final String OR="|";
    protected static final String AND="&";
    protected static final String NOT="!";         
    protected static final String LESS_EQ="<=";
    protected static final String LESS="<";
    protected static final String GREAT_EQ=">=";
    protected static final String GREAT=">";
    protected static final String DIFF="!=";
    protected static final String EQUA="=";        
    protected static final String SOUS="-";
    protected static final String ADD="+";
    protected static final String DIV="/";
    protected static final String MULT="*";

    //priorit� d�croissante des connecteurs

    private static final String CONNECTIVE[]={MULT,DIV,ADD,SOUS,
					      EQUA,DIFF,GREAT,LESS,GREAT_EQ,LESS_EQ,
					      NOT,AND,OR,IMPLY,EQUI,};  
    
    //PREPARATION DE LA FORMULE
    
    //indique si le mot est pr�fixe d'un connecteur
    
    private static boolean isPrefixeOfConnective(String s){
	for(int i=0;i<CONNECTIVE.length;i++)
	    if(CONNECTIVE[i].startsWith(s))
		return true;
	return false;
    }    
    
    //d�composition des termes de la formule 

    private static String[] stringTable(String formula){
	//La formule avec les blancs
	String s="";
	//Le mot courant trait�
	String w="";
	//Le charact�re courant trait�
	char c;
	for(int i=0;i<formula.length();i++){
	    c=formula.charAt(i);	  
	    //charact�re blanc ou parenth�se = on enregistre le mots et le caract�re
	    if(c==' ' || c==OPEN || c==CLOSE){
		s+=" "+w+" "+c;
		w="";
		continue;
	    }
	    //si le mot est un connecteur ou un terminal que l'on ne peut pas
	    //prolonger alors on l'enregistre.
	    if((isPrefixeOfConnective(w) && !isPrefixeOfConnective(w+c)) |
	       (!isPrefixeOfConnective(w) && isPrefixeOfConnective(""+c))){
		s+=" "+w;
		w=""+c;
		continue;
	    }
	    //dans les autres cas, on augmente la taille du mot
	    w=w+c;
	}
	//on enregistre le dernier mot
	s+=" "+w;
	//on met le tout dans un tableau
	StringTokenizer tokenizer=new StringTokenizer(s);
	String table[]=new String[tokenizer.countTokens()];
	for(int i=0;i<table.length;i++)
	    table[i]=tokenizer.nextToken();
	return table;
    }

    //CONSTRUCTION DE L'ARBRE SYNTAXIQUE
    
    //retourne la priorit� du connecteur s et retourne -1 si s n'est
    //pas un connecteur

    private static int getPriority(String s){
	for(int i=0;i<CONNECTIVE.length;i++)
	    if(CONNECTIVE[i].equals(s))
		return i;
	return -1;
    }
    
    //indique si s est un nombre
    
    private static boolean isNumber(String s){
	for(int i=0;i<s.length();i++)
	    if(NUMBER.indexOf(s.charAt(i))<0)
		return false;
	return true;
    }

    //indique si est un mot (une variable)
    
    private static boolean isWord(String s){
	if(ALPHA.indexOf(s.charAt(0))<0)
	    return false;
	for(int i=1;i<s.length();i++)
	    if(ALPHA.indexOf(s.charAt(i))<0 && NUMBER.indexOf(s.charAt(i))<0)
		return false;
	return true;
    }
    
    //retourne l'indice du connecteur de priorit� minimal qui n'est
    //inclu dans aucune parenth�se et -1 sinon

    private static int getMinimalConnective(String formula[],int start,int end){
	int connective=-1;//Position de connecteur
	int current;//connecteur courant
	int priority=-1;//Sa priorit�
	int deep=0;//Profondeur dans les parenth�ses
	for(int i=start;i<=end;i++){
	    if(formula[i].equals(""+OPEN)){
		deep++;
		continue;
	    }
	    if(formula[i].equals(""+CLOSE)){
		deep--;
		continue;
	    }
	    if(deep==0 && (current=getPriority(formula[i]))>priority){
		connective=i;
		priority=current;
	    }
	}
	return connective;
    }

    //construit un table o� chaque case est un mot de s; les
    //caract�res permettant de s�parer les mots sont tous les les
    //caract�res qui ne sont pas dans ALPHA, NUMBER et CONNECTIVE

    private static Formula read(String formula[], int start, int end) throws Exception{
	if(start==end){
	    //si start=end, c'est un cas de base, un atome
	    if(isWord(formula[start])){
		return new Var(formula[start]);
	    }
	    if(isNumber(formula[start]))
		return new Int(Integer.parseInt(formula[start]));
	}else{
	    //Sinon, on recherche le connecteur de priorit� minimal qui
	    //est contenu dans aucune parenth�se et on fait un appel
	    //recursif
	    int connective=getMinimalConnective(formula,start,end);
	    if(connective<0){
		//Si pas de connecteur alors start est une parenth�se
		//ouvrante dont la parenth�se fermante est end
		if(start+1<end)
		    return read(formula,start+1,end-1);
	    }
	    else{
		//On regarde si le connecteur est Not, le seule qui est unaire
		if(formula[connective].equals(NOT) && start==connective && connective<end)
		    return new Not(read(formula,connective+1,end));
		//Sinon, on check tous les autres connecteurs
		if(start<connective && connective<end){
		    Formula left=read(formula,start,connective-1);
		    Formula right=read(formula,connective+1,end);
		    if(formula[connective].equals(EQUI))
			return new Equi(left,right);
		    if(formula[connective].equals(IMPLY))
			return new Imply(left,right);
		    if(formula[connective].equals(OR))
			return new Or(left,right);
		    if(formula[connective].equals(AND))
			return new And(left,right);
		    if(formula[connective].equals(LESS_EQ))
			return new LessEq(left,right);
		    if(formula[connective].equals(LESS))
			return new Less(left,right);
		    if(formula[connective].equals(GREAT_EQ))
			return new GreatEq(left,right);
		    if(formula[connective].equals(GREAT))
			return new Great(left,right);
		    if(formula[connective].equals(DIFF))
			return new Diff(left,right);
		    if(formula[connective].equals(EQUA))
			return new Equa(left,right);
		    if(formula[connective].equals(SOUS))
			return new Sous(left,right);
		    if(formula[connective].equals(ADD))
			return new Add(left,right);
		    if(formula[connective].equals(DIV))
			return new Div(left,right);
		    if(formula[connective].equals(MULT))
			return new Mult(left,right);
		}
	    }
	}
	throw new Exception("error syntax in subformula : "+subFormula(formula,start,end));
    }
    
    //retourne la formule comprise entre start et end

    private static String subFormula(String formula[],int start,int end){
	String sub=formula[start];
	for(int i=start+1;i<=end;i++)
	    sub+=" "+formula[i];
	return sub;
    }
    
    //INTERFACE
    
    public static Formula parse(String s) throws Exception{
	String formula[]= Formula.stringTable(s);
	return read(formula,0,formula.length-1);
    }
    
}