

package bsh;

import java.net.*;
import java.util.*;
import java.io.IOException;
import java.io.*;



public abstract class BshClassManager
{
	
	private static BshClassManager manager;
	private static boolean checkedForManager;
	
	private static Object NOVALUE = new Object(); 
	
	
	private static ClassLoader externalClassLoader;

	
    protected transient static Hashtable absoluteClassCache = new Hashtable();
	
    protected transient static Hashtable absoluteNonClasses = new Hashtable();

	

	


	public static BshClassManager getClassManager() 
	{
		

		
		if ( !checkedForManager && manager == null )
			
			try {
				if ( plainClassForName("java.lang.ref.WeakReference") != null
					&& plainClassForName("java.util.HashMap")  != null )
				{
					
					Class bcm = plainClassForName(
						"bsh.classpath.ClassManagerImpl");
					manager = (BshClassManager)bcm.newInstance();
				}
			} catch ( ClassNotFoundException e ) {
				
			} catch ( Exception e ) {
				System.err.println("Error loading classmanager: "+e);
			}

		checkedForManager = true;
		return manager;
	}

	public static boolean classExists( String name ) {
		return ( classForName( name ) != null );
	}

	
	public static Class classForName( String name ) {
		BshClassManager manager = getClassManager(); 
		if ( manager != null )
			return manager.getClassForName( name );
		else
			try {
				return plainClassForName( name );
			} catch ( ClassNotFoundException e ) {
				return null;
			}
	}

	
	public static Class plainClassForName( String name ) 
		throws ClassNotFoundException 
	{
		try {
			Class c;
			if ( externalClassLoader != null )
				c = externalClassLoader.loadClass( name );
			else {
				
				BshClassManager bcm = manager; 
				if ( bcm != null )
					c = bcm.getPlainClassForName( name );				
				else
					c = Class.forName( name );
			}

			cacheClassInfo( name, c );
			return c;
		
		} catch ( NoClassDefFoundError e ) {
			cacheClassInfo( name, null ); 
			throw new ClassNotFoundException( e.toString() );
		}
	}

	
	public static void cacheClassInfo( String name, Class value ) {
		if ( value != null )
			absoluteClassCache.put( name, value );
		else
			absoluteNonClasses.put( name, NOVALUE );
	}

	
	protected void clearCaches() {
    	absoluteNonClasses = new Hashtable();
    	absoluteClassCache = new Hashtable();
	}

	
	public static void addCMListener( Listener l ) {
		getClassManager(); 
		if ( manager != null )
			manager.addListener( l );
	}

	
	public static void setClassLoader( ClassLoader externalCL ) 
	{
		externalClassLoader = externalCL;
		BshClassManager bcm = getClassManager();
		if ( bcm != null )
			bcm.classLoaderChanged();
	}

	

	public static interface Listener 
	{
		public void classLoaderChanged();
	}

	

	
	public abstract Class getClassForName( String name );

	
	public abstract Class getPlainClassForName( String name ) 
		throws ClassNotFoundException ;

	public abstract ClassLoader getBaseLoader();

	public abstract ClassLoader getLoaderForClass( String name );

	public abstract void addClassPath( URL path )
		throws IOException;

	
	public abstract void reset();

	
	public abstract void setClassPath( URL [] cp );

	
	public abstract void reloadAllClasses() throws ClassPathException;

	
	public abstract void reloadClasses( String [] classNames )
		throws ClassPathException;

	
	public abstract void reloadPackage( String pack ) 
		throws ClassPathException ;

	

	
	public abstract void doSuperImport() throws EvalError;

	
	public abstract String getClassNameByUnqName( String name ) 
		throws ClassPathException;

	public abstract void addListener( Listener l );

	public abstract void removeListener( Listener l );

	public abstract void dump( PrintWriter pw );

	protected abstract void classLoaderChanged();
}
