package net.sf.jabref; 

import java.util.Comparator; 
import java.util.List; 
import java.util.Iterator; 


public  class  FieldComparatorStack <T> implements  Comparator ,  Comparator<T> {
	

    

	

    public FieldComparatorStack(List<? extends Comparator<? super T>> comparators) {
        this.comparators = comparators;
    }


	

    


	

    List<? extends Comparator<? super T>> comparators;

	

    public int compare(T o1, T o2) {
    	for (Comparator<? super T> comp : comparators){
    		int res = comp.compare(o1, o2);
            if (res != 0)
                return res;
        }
        return 0;
    }


}
