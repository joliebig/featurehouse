

package org.gjt.sp.jedit.bsh;

import java.util.Enumeration;
import java.util.Vector;
import java.util.Hashtable;
import java.lang.reflect.Array;


public class CollectionManager
{
    private static CollectionManager manager;

    public synchronized static CollectionManager getCollectionManager()
    {
        if ( manager == null
            && Capabilities.classExists("java.util.Collection") )
        {
            Class clas;
            try {
                clas = Class.forName( "org.gjt.sp.jedit.bsh.collection.CollectionManagerImpl" );
                manager = (CollectionManager)clas.newInstance();
            } catch ( Exception e ) {
                Interpreter.debug("unable to load CollectionManagerImpl: "+e);
            }
        }

        if ( manager == null )
            manager = new CollectionManager(); 

        return manager;
    }

    
    public boolean isBshIterable( Object obj )
    {
        
        try {
            getBshIterator( obj );
            return true;
        } catch( IllegalArgumentException e ) {
            return false;
        }
    }

    public BshIterator getBshIterator( Object obj )
        throws IllegalArgumentException
    {
        return new BasicBshIterator( obj );
    }

    public boolean isMap( Object obj ) {
        return obj instanceof Hashtable;
    }

    public Object getFromMap( Object map, Object key ) {
        return ((Hashtable)map).get(key);
    }

    public Object putInMap( Object map, Object key, Object value )
    {
        return ((Hashtable)map).put(key, value);
    }

    

    
    public static class BasicBshIterator implements BshIterator
    {
        Enumeration enumeration;

        
        public BasicBshIterator(Object iterateOverMe) {
            enumeration = createEnumeration(iterateOverMe);
        }

        
        protected Enumeration createEnumeration( Object iterateOverMe )
        {
            if(iterateOverMe==null)
                throw new NullPointerException("Object arguments passed to " +
                    "the BasicBshIterator constructor cannot be null.");

            if (iterateOverMe instanceof Enumeration)
                return (Enumeration)iterateOverMe;

            if (iterateOverMe instanceof Vector)
                return ((Vector)iterateOverMe).elements();

            if (iterateOverMe.getClass().isArray()) {
                final Object array = iterateOverMe;
                return new Enumeration() {
                    int index = 0, length = Array.getLength(array);
                    public Object nextElement() {
                        return Array.get(array, index++);
                    }
                    public boolean hasMoreElements() { return index<length; }
                };
            }

            if (iterateOverMe instanceof String)
                return createEnumeration(((String)iterateOverMe).toCharArray());

            if (iterateOverMe instanceof StringBuffer)
                return createEnumeration(
                    iterateOverMe.toString().toCharArray());

            if (iterateOverMe instanceof StringBuilder)
                return createEnumeration(
                    iterateOverMe.toString().toCharArray());

            throw new IllegalArgumentException(
                "Cannot enumerate object of type "+iterateOverMe.getClass());
        }

        
        public Object next() {
            return 	enumeration.nextElement();
        }

        
        public boolean hasNext() {
            return enumeration.hasMoreElements();
        }
    }
}
