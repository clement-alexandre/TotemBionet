package jlogic;

public class Equa extends BinConnective{

    public Equa(Formula left, Formula right){
	super(left,right);
    }
    
    public int eval(){
	if(getLeft().eval()==getRight().eval())
	    return 1;
	return 0;
    }
    
    public String toString(){
	return "("+getLeft()+ Formula.EQUA+getRight()+")";
    }

}