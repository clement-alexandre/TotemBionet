package jlogic;

public class Div extends BinConnective{

    public Div(Formula left, Formula right){
	super(left,right);
    }
    
    public int eval(){
	return getLeft().eval()/getRight().eval();
    }
    
    public String toString(){
	return "("+getLeft()+ Formula.DIV+getRight()+")";
    }

}