package jlogic;

public class Equi extends BinConnective{

    public Equi(Formula left, Formula right){
	super(left,right);
    }

    public int eval(){
	int left=getLeft().eval();
	int right=getRight().eval();
	if((left>0 && right>0)||(left<1 && right<1))
	    return 1;
	return 0;
    }

    public String toString(){
	return "("+getLeft()+ Formula.EQUI+getRight()+")";
    }

}