

package bsh.classpath;

import java.net.*;
import java.util.*;
import java.lang.ref.*;
import java.io.IOException;
import java.io.*;
import bsh.classpath.BshClassPath.ClassSource;
import bsh.classpath.BshClassPath.JarClassSource;
import bsh.classpath.BshClassPath.GeneratedClassSource;
import bsh.BshClassManager;
import bsh.ClassPathException;
import bsh.Interpreter;  
import bsh.UtilEvalError; 


public class ClassManagerImpl extends BshClassManager
{
	static final String BSH_PACKAGE = "bsh";
	
	private BshClassPath baseClassPath;
	private boolean superImport;

	
	private BshClassPath fullClassPath;

	
	private Vector listeners = new Vector();
	private ReferenceQueue refQueue = new ReferenceQueue();

	
	private BshClassLoader baseLoader;

	
	private Map loaderMap;

	
	public ClassManagerImpl() {
		reset();
	}

	
	public Class classForName( String name )
	{
		
		Class c = (Class)absoluteClassCache.get(name);
		if (c != null )
			return c;

		
		if ( absoluteNonClasses.get(name)!=null ) {
			if ( Interpreter.DEBUG )
				Interpreter.debug("absoluteNonClass list hit: "+name);
			return null;
		}

		if ( Interpreter.DEBUG )
			Interpreter.debug("Trying to load class: "+name);

		
		ClassLoader overlayLoader = getLoaderForClass( name );
		if ( overlayLoader != null )
		{
			try {
				c = overlayLoader.loadClass(name);
			} catch ( Exception e ) {
			
			
			} catch ( NoClassDefFoundError e2 ) {
				throw noClassDefFound( name, e2 );
			}

			
			
		}

		
		if ( c == null ) {
			if ( name.startsWith( BSH_PACKAGE ) )
				try {
					c = Interpreter.class.getClassLoader().loadClass( name );
				} catch ( ClassNotFoundException e ) {}
		}

		
		if ( c == null ) {
			if ( baseLoader != null )
				try {
					c = baseLoader.loadClass( name );
				} catch ( ClassNotFoundException e ) {}
		}

		
		if ( c == null ) {
			if ( externalClassLoader != null )
				try {
					c = externalClassLoader.loadClass( name );
				} catch ( ClassNotFoundException e ) {}
		}

		
		
		
		
		if ( c ==  null )
		{
			try {
				ClassLoader contextClassLoader = 
					Thread.currentThread().getContextClassLoader();
				if ( contextClassLoader != null )
					c = Class.forName( name, true, contextClassLoader );
			} catch ( ClassNotFoundException e ) { 
			} catch ( SecurityException e ) { } 
		}

		
		if ( c == null )
			try {
				c = plainClassForName( name );
			} catch ( ClassNotFoundException e ) {}

		
		if ( c == null )
			c = loadSourceClass( name );

		
		
		
		cacheClassInfo( name, c );

		return c;
	}

	
	public URL getResource( String path ) 
	{
		URL url = null;
		if ( baseLoader != null )
			
			url = baseLoader.getResource( path.substring(1) );
		if ( url == null )
			url = super.getResource( path );
		return url;
	}

	
	public InputStream getResourceAsStream( String path ) 
	{
		InputStream in = null;
		if ( baseLoader != null )
		{
			
			in = baseLoader.getResourceAsStream( path.substring(1) );
		}
		if ( in == null )
		{
			in = super.getResourceAsStream( path );
		}
		return in;
	}

	ClassLoader getLoaderForClass( String name ) {
		return (ClassLoader)loaderMap.get( name );
	}

	

	
	public void addClassPath( URL path ) 
		throws IOException 
	{
		if ( baseLoader == null )
			setClassPath( new URL [] { path } );
		else {
			
			baseLoader.addURL( path );
			baseClassPath.add( path );
			classLoaderChanged();
		}
	}

	
	public void reset()
	{
		baseClassPath = new BshClassPath("baseClassPath");
		baseLoader = null;
		loaderMap = new HashMap();
		classLoaderChanged(); 
	}

	
	public void setClassPath( URL [] cp ) {
		baseClassPath.setPath( cp );
		initBaseLoader();
		loaderMap = new HashMap();
		classLoaderChanged();
	}

	
	public void reloadAllClasses() throws ClassPathException 
	{
		BshClassPath bcp = new BshClassPath("temp");
		bcp.addComponent( baseClassPath );
		bcp.addComponent( BshClassPath.getUserClassPath() );
		setClassPath( bcp.getPathComponents() );
	}

	
	private void initBaseLoader() {
		baseLoader = new BshClassLoader( this, baseClassPath );
	}

	

	
	public void reloadClasses( String [] classNames ) 
		throws ClassPathException
	{
		

		
		if ( baseLoader == null )
			initBaseLoader();

		DiscreteFilesClassLoader.ClassSourceMap map = 
			new DiscreteFilesClassLoader.ClassSourceMap();

		for (int i=0; i< classNames.length; i++) {
			String name = classNames[i];

			
			ClassSource classSource = baseClassPath.getClassSource( name );

			
			if ( classSource == null ) {
				BshClassPath.getUserClassPath().insureInitialized();
				classSource = BshClassPath.getUserClassPath().getClassSource( 
					name );
			}

			
			
				
			if ( classSource == null )
				throw new ClassPathException("Nothing known about class: "
					+name );

			
			
			
			if ( classSource instanceof JarClassSource )
				throw new ClassPathException("Cannot reload class: "+name+
					" from source: "+ classSource );

			map.put( name, classSource );
		}

		
		ClassLoader cl = new DiscreteFilesClassLoader( this, map );

		
		Iterator it = map.keySet().iterator();
		while ( it.hasNext() )
			loaderMap.put( (String)it.next(), cl );

		classLoaderChanged();
	}

	
	public void reloadPackage( String pack ) 
		throws ClassPathException 
	{
		Collection classes = 
			baseClassPath.getClassesForPackage( pack );

		if ( classes == null )
			classes = 
				BshClassPath.getUserClassPath().getClassesForPackage( pack );

		

		if ( classes == null )
			throw new ClassPathException("No classes found for package: "+pack);

		reloadClasses( (String[])classes.toArray( new String[0] ) );
	}

	

	

	
	public BshClassPath getClassPath() throws ClassPathException
	{
		if ( fullClassPath != null )
			return fullClassPath;
	
		fullClassPath = new BshClassPath("BeanShell Full Class Path");
		fullClassPath.addComponent( BshClassPath.getUserClassPath() );
		try {
			fullClassPath.addComponent( BshClassPath.getBootClassPath() );
		} catch ( ClassPathException e ) { 
			System.err.println("Warning: can't get boot class path");
		}
		fullClassPath.addComponent( baseClassPath );

		return fullClassPath;
	}

	
	public void doSuperImport() 
		throws UtilEvalError
	{
		

		try {
			getClassPath().insureInitialized();
			
			getClassNameByUnqName( "" ) ;

			
			

		} catch ( ClassPathException e ) {
			throw new UtilEvalError("Error importing classpath "+ e );
		}

		superImport = true;
	}

	protected boolean hasSuperImport() { return superImport; }

	
	public String getClassNameByUnqName( String name ) 
		throws ClassPathException
	{
		return getClassPath().getClassNameByUnqName( name );
	}

	public void addListener( Listener l ) {
		listeners.addElement( new WeakReference( l, refQueue) );

		
		Reference deadref;
		while ( (deadref = refQueue.poll()) != null ) {
			boolean ok = listeners.removeElement( deadref );
			if ( ok ) {
				
			} else {
				if ( Interpreter.DEBUG ) Interpreter.debug(
					"tried to remove non-existent weak ref: "+deadref);
			}
		}
	}

	public void removeListener( Listener l ) {
		throw new Error("unimplemented");
	}

	public ClassLoader getBaseLoader() {
		return baseLoader;
	}

	

	
	public Class defineClass( String name, byte [] code ) 
	{
		baseClassPath.setClassSource( name, new GeneratedClassSource( code ) );
		try {
			reloadClasses( new String [] { name } );
		} catch ( ClassPathException e ) {
			throw new bsh.InterpreterError("defineClass: "+e);
		}
		return classForName( name );
	}

	
	protected void classLoaderChanged() 
	{
		
		clearCaches();

		Vector toRemove = new Vector(); 
		for ( Enumeration e = listeners.elements(); e.hasMoreElements(); ) 
		{
			WeakReference wr = (WeakReference)e.nextElement();
			Listener l = (Listener)wr.get();
			if ( l == null )  
			  toRemove.add( wr );
			else
			  l.classLoaderChanged();
		}
		for( Enumeration e = toRemove.elements(); e.hasMoreElements(); ) 
			listeners.removeElement( e.nextElement() );
	}

	public void dump( PrintWriter i ) 
	{
		i.println("Bsh Class Manager Dump: ");
		i.println("----------------------- ");
		i.println("baseLoader = "+baseLoader);
		i.println("loaderMap= "+loaderMap);
		i.println("----------------------- ");
		i.println("baseClassPath = "+baseClassPath);
	}

}
