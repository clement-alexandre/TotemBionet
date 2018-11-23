package jlogic;

public class Add extends BinConnective{

    public Add(Formula left, Formula right){
	super(left,right);
    }
    
    public int eval(){
	return getLeft().eval()+getRight().eval();
    }
    
    public String toString(){
	return "("+getLeft()+ Formula.ADD+getRight()+")";
    }
    
}