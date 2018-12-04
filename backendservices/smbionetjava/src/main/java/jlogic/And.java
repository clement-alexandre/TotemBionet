package jlogic;

public class And extends BinConnective{
    
    public And(Formula left, Formula right){
	super(left,right);
    }
    
    public int eval(){
	if(getLeft().eval()>0 && getRight().eval()>0)
	    return 1;
	return 0;
    }
            
    public String toString(){
	return "("+getLeft()+ Formula.AND+getRight()+")";
    }

}