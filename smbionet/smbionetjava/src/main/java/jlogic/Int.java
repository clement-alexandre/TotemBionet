package jlogic;

public class Int extends Formula {
    
    public final int i;

    public Int(int i){
	this.i=i;
    }

    public int eval(){
	return i;
    }
    
    public String toString(){
	return i+"";
    }
    
    public boolean insert(Var v){
	return false;
    }
    
    public void set(String name,int level){}
    
    public void setColor(int c){}

    public boolean colorOfAllVarIs(int c){
	return true;
    }
    
}