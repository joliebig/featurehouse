package bsh.collection;

import java.util.Iterator;
import java.util.Collection;
import java.util.Enumeration;


import java.lang.reflect.Array;


public class CollectionIterator implements bsh.BshIterator
{
	private Iterator iterator;

		
	public CollectionIterator(Object iterateOverMe) {
		iterator = createIterator(iterateOverMe);	
	}

	
	protected Iterator createIterator(Object iterateOverMe) 
	{
		if (iterateOverMe==null)
			throw new NullPointerException("Object arguments passed to " +
				"the CollectionIterator constructor cannot be null.");

		if (iterateOverMe instanceof Iterator)
			return (Iterator)iterateOverMe;

		if (iterateOverMe instanceof Collection) 
			return ((Collection)iterateOverMe).iterator();

		

		throw new IllegalArgumentException(
			"Cannot enumerate object of type "+iterateOverMe.getClass());
	}

		
	public Object next() {
		return iterator.next();
	}
	
		
	public boolean hasNext() {
		return iterator.hasNext();	
	}
}
