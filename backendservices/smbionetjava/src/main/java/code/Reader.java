package code;

import jlogic.Formula;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.StreamTokenizer;

public class Reader {

    //Charact�res sp�ciaux
    private static final char DEC='=';    
    private static final char TO='>';
    private static final char COM='#';    
    private static final char OPEN='[';
    private static final char CLOSE=']';
    private static final char PLUS='+';
    private static final char EOD=';';

    //Mots cl�s
    private static final String VAR="VAR";
    private static final String REG="REG";
    private static final String PARA="PARA";
    private static final String CTL="CTL";
    private static final String KEY[]={VAR,REG,PARA,CTL};
    
    //Raccourcis
    private static final int EOF=StreamTokenizer.TT_EOF;
    private static final int WORD=StreamTokenizer.TT_WORD;
    private static final int NUMB=StreamTokenizer.TT_NUMBER;    

    //Indique si le string est un mot cl�
    
    private static boolean isKey(String s){
	for(int i=0;i<KEY.length;i++)
	    if(KEY[i].equals(s))
		return true;
	return false;
    }

    //Intialisation du stream

    private static void initStreamTokenizer(StreamTokenizer t,String wordChars){
	for(int i=0;i<256;i++)
	    if(wordChars.indexOf(i)>=0)
		t.wordChars(i,i);
	    else
		t.ordinaryChar(i);
	t.whitespaceChars((int)'\t',(int)'\t');
	t.whitespaceChars((int)' ',(int)' ');
	t.whitespaceChars((int)'\n',(int)'\n');
	t.commentChar(COM);	
    }
    
    //D�placement d'un stream jusqu'� au mot key
    
    private static void goTo(StreamTokenizer t,String key)throws Exception{
	//on se d�place au mot clez S.PARA
	while(!key.equals(t.sval))
	    if(t.nextToken()==EOF)
		throw new Exception("there is no key word "+key);
    }

    //Token suivant. Si le caract�re OPEN est rencontr� alors
    //t.ttype==OPEN et t.sval est la string strictement compris entre
    //OPEN et le premier caract�re CLOSE rencontr�. Le token suivant
    //sera le token suiant ce caract�re CLOSE
    
    private static int next(StreamTokenizer t)throws Exception{
	t.nextToken();
	if(t.ttype==OPEN){
	    String s="";
	    while(t.nextToken()!=CLOSE)
		if(t.ttype==WORD)
		    s+=t.sval;
		else
		    if(t.ttype==NUMB)
			s+=(int)t.nval;
		    else
			s+=(char)t.ttype;
	    t.sval=s;
	    t.ttype=OPEN;
	}
	return t.ttype;
    }
    
    //Lecture du bloc VAR

    public static void readVar(String f,Net net)throws Exception{
	BufferedReader br=new BufferedReader(new InputStreamReader(new FileInputStream(f)));
	StreamTokenizer t=new StreamTokenizer(br);
	initStreamTokenizer(t, Formula.ALPHA+ Formula.NUMBER);
	try{
	    //On se d�place au mot clez VAR
	    goTo(t,VAR);
	    //Lectures des g�nes
	    while(next(t)==WORD && !isKey(t.sval)){
		//Enregistrement du nom
		String name=t.sval;
		//Caract�re DEC
		if(next(t)!=DEC)
		    throw new Exception("Char "+DEC+" misses after gene "+name);
		//Lecture du min
		if(next(t)!=NUMB)
		    throw new Exception("There is no min value for "+name);
		int min=(int)t.nval;
		//Lecture du max
		if(next(t)!=NUMB)
		    throw new Exception("There is no max value for "+name);
		int max=(int)t.nval;
		//Ajout du g�ne dans le r�seau
		net.add(new Gene(name,min,max));
		//Fin de d�claration
		if(next(t)!=EOD)
		    throw new Exception("No char "+EOD);
	    }
	    //La sortie de la boucle se fait avec le bloc REG
	    if(!REG.equals(t.sval))
		throw new Exception("Syntax error");
	}catch(Exception e){
	    throw new Exception("Line "+t.lineno()+" : "+e.getMessage());
	}  
    }
    
    //Lecture du bloc REG
    
    public static void readReg(String f,Net net)throws Exception{
	BufferedReader br=new BufferedReader(new InputStreamReader(new FileInputStream(f)));
	StreamTokenizer t=new StreamTokenizer(br);
	initStreamTokenizer(t, Formula.ALPHA+ Formula.NUMBER);
	t.wordChars(PLUS,PLUS);
	try{
	    //On se d�place au mot clez REG
	    goTo(t,REG);
	    //Lectures des regulations
	    while(next(t)==WORD && !isKey(t.sval)){
		//Enregistrement du nom
		String name=t.sval;
		//Parenth�se ouvrante OPEN
		if(next(t)!=OPEN)
		    throw new Exception("Error when parsing the formula "+name);
		//Enregistrement de la formule
		Formula formula;
		try{
		    formula= Formula.parse(t.sval);
		}catch(Exception e){
		    throw new Exception("Syntaxe error in the formula");
		}
		//cr�ation de la r�gulation
		Reg r=new Reg(name,formula);
		//Caract�res DEC TO (=>)
		if(next(t)!=DEC || next(t)!=TO)
		    throw new Exception("Symbole "+DEC+TO+" misses after the formula");
		//Lecture des cibles
		Gene target;
		while(next(t)==WORD){
		    if((target=net.get(t.sval))==null)
			throw new Exception("Unknown gene "+t.sval);
		    //On ajoute la r�gulation dans la liste des
		    //r�gulation de la cible
		    target.regs.add(r);
		}
		//La sortie de la boucle se fait par une fin de d�claration
		if(t.ttype!=EOD)
		    throw new Exception("Syntax error");
	    }
	    //La sortie de la boucle se fait avec le bloc PARA, le bloc CTL ou la fin
	    if(t.ttype!=EOF && !PARA.equals(t.sval) && !CTL.equals(t.sval))
		throw new Exception("Syntax error");
	}catch(Exception e){
	    throw new Exception("Line "+t.lineno()+" : "+e.getMessage());
	}   
    }

    //Lecture du block PARA 
    
    public static boolean readPara(String f,Net net)throws Exception{
	BufferedReader br=new BufferedReader(new InputStreamReader(new FileInputStream(f)));
	StreamTokenizer t=new StreamTokenizer(br);
	initStreamTokenizer(t, Formula.ALPHA+ Formula.NUMBER+PLUS);
	try{
	    //On se d�place au mot mot clez PARA o� au ki�me mot clez MODEL
	    try{goTo(t,PARA);}catch(Exception e){return false;}
	    //Lectures des param�tres
	    while(next(t)==WORD && !isKey(t.sval)){
		//On recherche le param�tre de nom t.sval 
		Para p=null;
		for(int i=0;i<net.size();i++)
		    if((p=net.get(i).paras.get(t.sval))!=null)
			break;
		//Si le param�tre n'existe pas, c'est une erreur
		if(p==null)
		    throw new Exception("Unknown parameter "+t.sval);
		//Caract�re DEC
		if(next(t)!=DEC)
		    throw new Exception("Char "+DEC+" misses after parameter "+p);
		//Lecture du min
		if(next(t)!=NUMB)
		    throw new Exception("There is no min value for "+p);
		int min=(int)t.nval;
		//Si fin de d�claration min=max
		if(next(t)==EOD){
		    if(!p.setMinMax(min,min))
			throw new Exception("Bad value for "+p);
		}
		//Sinon, on a le max
		else{
		    //Le max est normalement d�j� lu
		    if(t.ttype!=NUMB)
			throw new Exception("There is no max value for "+p);
		    if(!p.setMinMax(min,(int)t.nval))
			throw new Exception("Bad value for "+p);
		    //Fin de d�claration
		    if(next(t)!=EOD)
			throw new Exception("No char "+EOD);
		}
	    }
	    //La sortie de la boucle se fait avec le bloc CTL, un
	    //autre model ou la fin
	    if(t.ttype!=EOF && !CTL.equals(t.sval))
		throw new Exception("Syntax error");
	    return true;
	}catch(Exception e){
	    throw new Exception("Line "+t.lineno()+" : "+e.getMessage());
	}   
    }

    //Lecture du bloc CTL

    public static String readCTL(String f)throws Exception{
	BufferedReader br=new BufferedReader(new InputStreamReader(new FileInputStream(f)));
	StreamTokenizer t=new StreamTokenizer(br);
	initStreamTokenizer(t, Formula.ALPHA);
	try{
	    //On se d�place au mot clez CTL
	    try{goTo(t,CTL);}catch(Exception e){return "";}
	    //On change le stream pour lire tous les charact�tes
	    for(int i=0;i<256;i++)
		t.ordinaryChar(i);
	    t.whitespaceChars((int)'\t',(int)'\t');
	    t.whitespaceChars((int)' ',(int)' ');
	    t.commentChar(COM);	
	    //Lecture de tous les caract�res et enregistrement dans ctl
	    String ctl="";
	    boolean eol=false;
	    while(t.nextToken()!=EOF)
		if(t.ttype=='\n')
		    eol=true;
		else
		    if(eol){
			ctl+="\n"+(char)t.ttype;
			eol=false;
		    }else
			ctl+=(char)t.ttype;
	    return ctl; 
	}catch(Exception e){
	    throw new Exception("Line "+t.lineno()+" : "+e.getMessage());
	}
    }
}