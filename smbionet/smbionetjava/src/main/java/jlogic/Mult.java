package jlogic;

public class Mult extends BinConnective{

    public Mult(Formula left, Formula right){
	super(left,right);
    }
    
    public int eval(){
	return getLeft().eval()*getRight().eval();
    }
    
    public String toString(){
	return "("+getLeft()+ Formula.MULT+getRight()+")";
    }

}