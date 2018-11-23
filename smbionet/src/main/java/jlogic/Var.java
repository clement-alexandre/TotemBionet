package jlogic;

public class Var extends Formula{
    
    //nom de la variable
    public final String name;
    //niveau courant de la variable
    public int level;
    //couleur de la variable
    private int color;

    //construction avec initialisation finale du nom
    
    public Var(String name){
	this.name=name;
    }
    
    //niveau courant
    
    public int getLevel(){
	return level;
    }

    //determination du niveau

    public void setLevel(int level){
	this.level=level;
    }
   

    //METHODES ABSTRAITES DE FORMULA
    
    public int eval(){
	return level;
    }
            
    public String toString(){
	return name;
    }
        
    public boolean insert(Var v){
	return false;
    }

    public void set(String name,int level){
	if(this.name.equals(name))
	    this.level=level;
    }

    public void setColor(int c){
	color=c;
    }

    public boolean colorOfAllVarIs(int c){
	return color==c;
    }

    
}