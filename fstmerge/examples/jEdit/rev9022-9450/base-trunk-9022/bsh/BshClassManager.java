

package bsh;

import java.net.*;
import java.util.*;
import java.io.IOException;
import java.io.*;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;



public class BshClassManager
{
	
	private static Object NOVALUE = new Object(); 
	
	private Interpreter declaringInterpreter;
	
	
	protected ClassLoader externalClassLoader;

	
    protected transient Hashtable absoluteClassCache = new Hashtable();
	
    protected transient Hashtable absoluteNonClasses = new Hashtable();

	
	protected transient Hashtable resolvedObjectMethods = new Hashtable();
	protected transient Hashtable resolvedStaticMethods = new Hashtable();

	protected transient Hashtable definingClasses = new Hashtable();
	protected transient Hashtable definingClassesBaseNames = new Hashtable();

	
	public static BshClassManager createClassManager( Interpreter interpreter ) 
	{
		BshClassManager manager;

		
		if ( Capabilities.classExists("java.lang.ref.WeakReference") 
			&& Capabilities.classExists("java.util.HashMap") 
			&& Capabilities.classExists("bsh.classpath.ClassManagerImpl") 
		) 
			try {
				
				
				Class clas = Class.forName( "bsh.classpath.ClassManagerImpl" );
				manager = (BshClassManager)clas.newInstance();
			} catch ( Exception e ) {
				throw new InterpreterError("Error loading classmanager: "+e);
			}
		else 
			manager = new BshClassManager();

		if ( interpreter == null )
			interpreter = new Interpreter();
		manager.declaringInterpreter = interpreter;
		return manager;
	}

	public boolean classExists( String name ) {
		return ( classForName( name ) != null );
	}

	
	public Class classForName( String name ) 
	{
		if ( isClassBeingDefined( name ) )
			throw new InterpreterError(
				"Attempting to load class in the process of being defined: "
				+name );

		Class clas = null;
		try {
			clas = plainClassForName( name );
		} catch ( ClassNotFoundException e ) {  }

		
		if ( clas == null ) 
			clas = loadSourceClass( name );

		return clas;
	}
	
	
	protected Class loadSourceClass( String name )
	{
		String fileName = "/"+name.replace('.','/')+".java";
		InputStream in = getResourceAsStream( fileName );
		if ( in == null )
			return null;

		try {
			System.out.println("Loading class from source file: "+fileName);
			declaringInterpreter.eval( new InputStreamReader(in) );
		} catch ( EvalError e ) {
			
			System.err.println( e );
		}
		try {
			return plainClassForName( name );
		} catch ( ClassNotFoundException e ) {
			System.err.println("Class not found in source file: "+name );
			return null;
		}
	}

	
	public Class plainClassForName( String name ) 
		throws ClassNotFoundException
	{
		Class c = null;

		try {
			if ( externalClassLoader != null )
				c = externalClassLoader.loadClass( name );
			else
				c = Class.forName( name );

			cacheClassInfo( name, c );

		
		} catch ( NoClassDefFoundError e ) {
			throw noClassDefFound( name, e );
		}

		return c;
	}

	
	public URL getResource( String path ) 
	{
		URL url = null;
		if ( externalClassLoader != null )
		{
			
			url = externalClassLoader.getResource( path.substring(1) );
		} 
		if ( url == null )
			url = Interpreter.class.getResource( path );

		return url;
	}
	
	public InputStream getResourceAsStream( String path ) 
	{
		InputStream in = null;
		if ( externalClassLoader != null )
		{
			
			in = externalClassLoader.getResourceAsStream( path.substring(1) );
		} 
		if ( in == null )
			in = Interpreter.class.getResourceAsStream( path );

		return in;
	}

	
	public void cacheClassInfo( String name, Class value ) {
		if ( value != null )
			absoluteClassCache.put( name, value );
		else
			absoluteNonClasses.put( name, NOVALUE );
	}

	
	public void cacheResolvedMethod( 
		Class clas, Class [] types, Method method ) 
	{
		if ( Interpreter.DEBUG )
			Interpreter.debug(
				"cacheResolvedMethod putting: " + clas +" "+ method );
		
		SignatureKey sk = new SignatureKey( clas, method.getName(), types );
		if ( Modifier.isStatic( method.getModifiers() ) )
			resolvedStaticMethods.put( sk, method );
		else
			resolvedObjectMethods.put( sk, method );
	}

	
	protected Method getResolvedMethod( 
		Class clas, String methodName, Class [] types, boolean onlyStatic  ) 
	{
		SignatureKey sk = new SignatureKey( clas, methodName, types );

		
		
		Method method = (Method)resolvedStaticMethods.get( sk );
		if ( method == null && !onlyStatic)
			method = (Method)resolvedObjectMethods.get( sk );

		if ( Interpreter.DEBUG )
		{
			if ( method == null )
				Interpreter.debug(
					"getResolvedMethod cache MISS: " + clas +" - "+methodName );
			else
				Interpreter.debug(
					"getResolvedMethod cache HIT: " + clas +" - " +method );
		}
		return method;
	}

	
	protected void clearCaches() 
	{
    	absoluteNonClasses = new Hashtable();
    	absoluteClassCache = new Hashtable();
    	resolvedObjectMethods = new Hashtable();
    	resolvedStaticMethods = new Hashtable();
	}

	
	public void setClassLoader( ClassLoader externalCL ) 
	{
		externalClassLoader = externalCL;
		classLoaderChanged();
	}

	public void addClassPath( URL path )
		throws IOException {
	}

	
	public void reset() { 
		clearCaches();
	}

	
	public void setClassPath( URL [] cp ) 
		throws UtilEvalError
	{
		throw cmUnavailable();
	}

	
	public void reloadAllClasses() throws UtilEvalError {
		throw cmUnavailable();
	}

	
	public void reloadClasses( String [] classNames )
		throws UtilEvalError 
	{
		throw cmUnavailable();
	}

	
	public void reloadPackage( String pack ) 
		throws UtilEvalError 
	{
		throw cmUnavailable();
	}

	

	
	protected void doSuperImport() 
		throws UtilEvalError 
	{
		throw cmUnavailable();
	}

	
	protected boolean hasSuperImport() 
	{
		return false;
	}

	
	protected String getClassNameByUnqName( String name ) 
		throws UtilEvalError 
	{
		throw cmUnavailable();
	}

	public void addListener( Listener l ) { }

	public void removeListener( Listener l ) { }

	public void dump( PrintWriter pw ) { 
		pw.println("BshClassManager: no class manager."); 
	}

	
	
	protected void definingClass( String className ) {
		String baseName = Name.suffix(className,1);
		int i = baseName.indexOf("$");
		if ( i != -1 )
			baseName = baseName.substring(i+1);
		String cur = (String)definingClassesBaseNames.get( baseName );
		if ( cur != null )
			throw new InterpreterError("Defining class problem: "+className 
				+": BeanShell cannot yet simultaneously define two or more "
				+"dependant classes of the same name.  Attempt to define: "
				+ className +" while defining: "+cur 
			);
		definingClasses.put( className, NOVALUE );
		definingClassesBaseNames.put( baseName, className );
	}

	protected boolean isClassBeingDefined( String className ) {
		return definingClasses.get( className ) != null;
	}

	
	protected String getClassBeingDefined( String className ) {
		String baseName = Name.suffix(className,1);
		return (String)definingClassesBaseNames.get( baseName );
	}

	
	protected void doneDefiningClass( String className ) {
		String baseName = Name.suffix(className,1);
		definingClasses.remove( className );
		definingClassesBaseNames.remove( baseName );
	}

	
	public Class defineClass( String name, byte [] code ) 
	{
		throw new InterpreterError("Can't create class ("+name
			+") without class manager package.");
	
	}

	protected void classLoaderChanged() { }

	
	protected static Error noClassDefFound( String className, Error e ) {
		return new NoClassDefFoundError(
			"A class required by class: "+className +" could not be loaded:\n"
			+e.toString() );
	}

	protected static UtilEvalError cmUnavailable() {
		return new Capabilities.Unavailable(
			"ClassLoading features unavailable.");
	}

	public static interface Listener 
	{
		public void classLoaderChanged();
	}

	
	
	static class SignatureKey
	{
		Class clas;
		Class [] types;
		String methodName;
		int hashCode = 0;

		SignatureKey( Class clas, String methodName, Class [] types ) {
			this.clas = clas;
			this.methodName = methodName;
			this.types = types;
		}

		public int hashCode() 
		{ 
			if ( hashCode == 0 ) 
			{
				hashCode = clas.hashCode() * methodName.hashCode();
				if ( types == null ) 
					return hashCode; 
				for( int i =0; i < types.length; i++ ) {
					int hc = types[i] == null ? 21 : types[i].hashCode();
					hashCode = hashCode*(i+1) + hc;
				}
			}
			return hashCode;
		}

		public boolean equals( Object o ) { 
			SignatureKey target = (SignatureKey)o;
			if ( types == null )
				return target.types == null;
			if ( clas != target.clas )
				return false;
			if ( !methodName.equals( target.methodName ) )
				return false;
			if ( types.length != target.types.length )
				return false;
			for( int i =0; i< types.length; i++ )
			{
				if ( types[i]==null ) 
				{
					if ( !(target.types[i]==null) )
						return false;
				} else 
					if ( !types[i].equals( target.types[i] ) )
						return false;
			}

			return true;
		}
	}
}
