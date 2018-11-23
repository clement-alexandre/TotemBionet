package jlist;

import java.util.ArrayList;

public class List<E> extends ArrayList<E>{

    //Retourne l'�l�ment de description s    
    
    public E get(String s){
	for(int i=0;i<size();i++)
	    if(get(i).toString().equals(s))
		return get(i);
	return null;
    }

    //Indique si la liste this et une sous-liste de l

    public boolean sublist(List<E> l){
	for(int i=0;i<size();i++)
	    if(!l.contains(get(i)))
		return false;
	return true;
    }

    //Indique si la liste this et une stricte sous-liste de l

    public boolean strictSublist(List<E> l){
	return sublist(l) && size()<l.size();
    }

    //Indique si la liste contient des �l�ments de m�me description
    
    public boolean hasRedundancy(){
	for(int i=0;i<size();i++)
	    for(int j=i+1;j<size();j++)
		if(get(i).toString().equals(get(j).toString()))
		    return true;
	return false;
    }

    //Addition d'un �l�ment dans l'ordre alphab�tique

    public void addAlpha(E e){
	int i;
	for(i=0;i<size();i++)
	    if(get(i).toString().compareTo(e.toString())>=0){
		add(i,e);
		break;
	    }
	if(i>=size())
	    add(e);
    }

}