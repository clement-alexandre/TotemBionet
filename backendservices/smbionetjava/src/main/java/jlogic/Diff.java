package jlogic;

public class Diff extends BinConnective {

    public Diff(Formula left, Formula right){
	super(left,right);
    }
    
    public int eval(){
	if(getLeft().eval()!=getRight().eval())
	    return 1;
	return 0;
    }
    
    public String toString(){
	return "("+getLeft()+ Formula.DIFF+getRight()+")";
    }

}