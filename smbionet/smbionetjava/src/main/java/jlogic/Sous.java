package jlogic;

public class Sous extends BinConnective{

    public Sous(Formula left, Formula right){
	super(left,right);
    }
    
    public int eval(){
	return getLeft().eval()-getRight().eval();
    }
    
    public String toString(){
	return "("+getLeft()+ Formula.SOUS+getRight()+")";
    }
    
}