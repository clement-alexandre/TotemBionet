package jlogic;

public class GreatEq extends BinConnective{

    public GreatEq(Formula left, Formula right){
	super(left,right);
    }
    
    public int eval(){
	if(getLeft().eval()>=getRight().eval())
	    return 1;
	return 0;
    }
    
    public String toString(){
	return "("+getLeft()+ Formula.GREAT_EQ+getRight()+")";
    }

}