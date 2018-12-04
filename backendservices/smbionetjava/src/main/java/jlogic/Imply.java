package jlogic;

public class Imply extends BinConnective{

    public Imply(Formula left, Formula right){
	super(left,right);
    }
    
    public int eval(){
	if(getLeft().eval()<1 || getRight().eval()>0)
	    return 1;
	return 0;
    }

    public String toString(){
	return "("+getLeft()+ Formula.IMPLY+getRight()+")";
    }

}