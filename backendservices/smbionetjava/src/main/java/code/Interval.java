package code;

public class Interval {

    public final int min;
    public final int max;
    private String name;
    
    public Interval(int min, int max){
	this.min=min;
	this.max=max;
	if(min==max)
	    name="("+min+")";
	else
	    name="("+min+"~"+max+")";
    }

    public Interval(int i, boolean special){
	this(i,i);
	if(special)
	    name="["+i+"]";
    }

    //Un interval A est "faiblement inf�rieur" � un interval B si
    //min(A)<=max(B).

    public boolean weakLess(Interval interval){
	return min<=interval.max;
    }
    
    public String toString(){
	return name;
    }
    
}