package jlogic;

public class Great extends BinConnective{

    public Great(Formula left, Formula right){
	super(left,right);
    }
    
    public int eval(){
	if(getLeft().eval()>getRight().eval())
	    return 1;
	return 0;
    }

    public String toString(){
	return "("+getLeft()+ Formula.GREAT+getRight()+")";
    }
    
}