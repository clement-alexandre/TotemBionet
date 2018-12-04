package jlogic;

abstract public class BinConnective extends Formula {

    private Formula right;
    private Formula left;
        
    public BinConnective(Formula left, Formula right){
	this.left=left;
	this.right=right;
    }

    public Formula getLeft(){
	return left;
    }

    public Formula getRight(){
	return right;
    }

    public void set(String name,int level){
	left.set(name,level);
	right.set(name,level);
    }

    public boolean insert(Var v){
	boolean leftInsert,rightInsert;
	if((left instanceof Var) && ((Var)left).name.equals(v.name)){
	    left=v;
	    leftInsert=true;
	}else
	    leftInsert=left.insert(v);
	if((right instanceof Var) && ((Var)right).name.equals(v.name)){
	    right=v;
	    rightInsert=true;
	}else
	    rightInsert=right.insert(v);
	return leftInsert || rightInsert;
    }

    public void setColor(int c){
	left.setColor(c);
	right.setColor(c);
    }

    public boolean colorOfAllVarIs(int c){
	return left.colorOfAllVarIs(c) &&  right.colorOfAllVarIs(c);
    }
    
}