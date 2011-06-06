

package bsh.collection;

import java.util.Iterator;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Map;
import java.lang.reflect.Array;
import bsh.BshIterator;


public class CollectionManagerImpl extends bsh.CollectionManager
{
	public BshIterator getBshIterator( Object obj ) 
		throws IllegalArgumentException
	{
		if ( obj instanceof Collection || obj instanceof Iterator )
			return new CollectionIterator( obj ); 
		else
			return new bsh.CollectionManager.BasicBshIterator( obj ); 
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
