package code;

import jlogic.Formula;
import jlogic.Var;

public class Reg {
    
    //Nom de la r�gulation
    public final String name;
    //Formule bool�enne (pr�sence/absence de la r�gulation)
    public final Formula formula;
    
    //Constructeur
    
    public Reg(String name, Formula formula)throws Exception{
	this.name=name;
	this.formula=formula;
	if(formula instanceof Var)
	    throw new Exception("The formula connot be reduced to a variable");
    }

    //La description = le nom
    
    public String toString(){
	return name;
    }
    
    //Indique si la formule est pr�sente

    public boolean isEffective(){
	return formula.eval()>0;
    }

}