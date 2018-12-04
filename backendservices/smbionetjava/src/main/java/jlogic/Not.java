package jlogic;

public class Not extends Formula {

    private Formula right;

    public Not(Formula right){
	this.right=right;
    }
    
    public int eval(){
	if(right.eval()>0)
	    return 0;
	return 1;
    }
    
    public String toString(){
	return "("+ Formula.NOT+right+")";
    }
    
    public boolean insert(Var v){
	if((right instanceof Var) && ((Var)right).name.equals(v.name)){
	    right=v;
	    return true;
	}else
	    return right.insert(v);
    }

    public void set(String name,int level){
	right.set(name,level);
    }

    public void setColor(int c){
	right.setColor(c);
    }

    public boolean colorOfAllVarIs(int c){
	return right.colorOfAllVarIs(c);
    }

}