package jclock;

public class Clock {

    private long t;
    
    public Clock(){
	start();
    }

    public void start(){
	t=System.currentTimeMillis();
    }

    public String toString(){
	long delta=System.currentTimeMillis()-t;
	long h=delta/3600000;
	delta-=h*3600000;
	long m=delta/60000;
	delta-=m*60000;
	long s=delta/1000;
	delta-=s*1000;
	String time;
	if(h>0)
	    time=h+"h"+m+"m";
	else
	    if(m>0)
		time=m+"m"+s+"s";
	    else
		if(s>0)
		    time=s+"s"+delta+"ms";
		else
		    time=delta+"ms";
	return time;
    }

}