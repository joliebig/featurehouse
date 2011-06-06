

package org.gjt.sp.jedit.bsh.collection;

import java.util.Iterator;
import java.util.Collection;
import java.util.Map;

import org.gjt.sp.jedit.bsh.BshIterator;


public class CollectionManagerImpl extends org.gjt.sp.jedit.bsh.CollectionManager
{
	public BshIterator getBshIterator( Object obj ) 
		throws IllegalArgumentException
	{
		if ( obj instanceof Collection || obj instanceof Iterator )
			return new CollectionIterator( obj ); 
		else
			return new org.gjt.sp.jedit.bsh.CollectionManager.BasicBshIterator( obj );
	}

	public boolean isMap( Object obj ) 
	{
		if ( obj instanceof Map )
			return true;
		else
			return super.isMap( obj );
	}

	public Object getFromMap( Object map, Object key ) 
	{
		
		return ((Map)map).get(key);
	}
	public Object putInMap( Object map, Object key, Object value ) 
	{
		
		return ((Map)map).put(key, value);
	}
}
