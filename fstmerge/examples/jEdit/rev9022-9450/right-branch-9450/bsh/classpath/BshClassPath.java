

package bsh.classpath;

import java.util.*;
import java.util.zip.*;
import java.io.*;
import java.net.*;
import java.io.File;
import bsh.ConsoleInterface;
import bsh.StringUtil;
import bsh.ClassPathException;
import java.lang.ref.WeakReference;
import bsh.NameSource;


public class BshClassPath 
	implements ClassPathListener, NameSource
{
	String name;

	
	private List path;
	
	private List compPaths;

	
	private Map packageMap;
	
	private Map classSource;
	
	private boolean mapsInitialized;

	private UnqualifiedNameTable unqNameTable;

	
	private boolean nameCompletionIncludesUnqNames = true;

	Vector listeners = new Vector();

	

	public BshClassPath( String name ) { 
		this.name = name;
		reset();
	}

	public BshClassPath(  String name, URL [] urls ) {
		this( name );
		add( urls );
	}

	

	

	public void setPath( URL[] urls ) {
		reset();
		add( urls );
	}

	
	public void addComponent( BshClassPath bcp ) { 
		if ( compPaths == null )
			compPaths = new ArrayList();
		compPaths.add( bcp );
		bcp.addListener( this );
	}

	public void add( URL [] urls ) { 
		path.addAll( Arrays.asList(urls) );
		if ( mapsInitialized )
			map( urls );
	}

	public void add( URL url ) throws IOException { 
		path.add(url);
		if ( mapsInitialized )
			map( url );
	}

	
	public URL [] getPathComponents() {
		return (URL[])getFullPath().toArray( new URL[0] );
	}

	
	synchronized public Set getClassesForPackage( String pack ) {
		insureInitialized();
		Set set = new HashSet();
		Collection c = (Collection)packageMap.get( pack );
		if ( c != null )
			set.addAll( c );

		if ( compPaths != null )
			for (int i=0; i<compPaths.size(); i++) {
				c = ((BshClassPath)compPaths.get(i)).getClassesForPackage( 
					pack );
				if ( c != null )
					set.addAll( c );
			}
		return set;
	}

	
	synchronized public ClassSource getClassSource( String className ) 
	{
		
		
		
		ClassSource cs = (ClassSource)classSource.get( className );
		if ( cs != null )
			return cs;

		insureInitialized(); 

		cs = (ClassSource)classSource.get( className );
		if ( cs == null && compPaths != null )
			for (int i=0; i<compPaths.size() && cs==null; i++)
				cs = ((BshClassPath)compPaths.get(i)).getClassSource(className);
		return cs;
	}

	
	synchronized public void setClassSource( String className, ClassSource cs ) 
	{
		classSource.put( className, cs );
	}

	
	public void insureInitialized() 
	{
		insureInitialized( true );
	}

	
	protected synchronized void insureInitialized( boolean topPath ) 
	{
		
		
		if ( topPath && !mapsInitialized )
			startClassMapping();

		
		if ( compPaths != null )
			for (int i=0; i< compPaths.size(); i++)
				((BshClassPath)compPaths.get(i)).insureInitialized( false );

		
		if ( !mapsInitialized ) 
			map( (URL[])path.toArray( new URL[0] ) );

		if ( topPath && !mapsInitialized )
			endClassMapping();

		mapsInitialized = true;
	}

	
	protected List getFullPath() 
	{
		List list = new ArrayList();
		if ( compPaths != null ) {
			for (int i=0; i<compPaths.size(); i++) {
				List l = ((BshClassPath)compPaths.get(i)).getFullPath();
				
				
				Iterator it = l.iterator();
				while ( it.hasNext() ) {
					Object o = it.next();
					if ( !list.contains(o) )
						list.add( o );
				}
			}
		}
		list.addAll( path );
		return list;
	}


	
	public String getClassNameByUnqName( String name ) 
		throws ClassPathException
	{
		insureInitialized();
		UnqualifiedNameTable unqNameTable = getUnqualifiedNameTable();

		Object obj = unqNameTable.get( name );
		if ( obj instanceof AmbiguousName )
			throw new ClassPathException("Ambigous class names: "+
				((AmbiguousName)obj).get() );

		return (String)obj;
	}

	
	private UnqualifiedNameTable getUnqualifiedNameTable() {
		if ( unqNameTable == null )
			unqNameTable = buildUnqualifiedNameTable();
		return unqNameTable;
	}

	private UnqualifiedNameTable buildUnqualifiedNameTable() 
	{
		UnqualifiedNameTable unqNameTable = new UnqualifiedNameTable();

		
		if ( compPaths != null )
			for (int i=0; i<compPaths.size(); i++) {
				Set s = ((BshClassPath)compPaths.get(i)).classSource.keySet();
				Iterator it = s.iterator();
				while(it.hasNext()) 
					unqNameTable.add( (String)it.next() );
			}

		
		Iterator it = classSource.keySet().iterator();
		while(it.hasNext()) 
			unqNameTable.add( (String)it.next() );
		
		return unqNameTable;
	}

	public String [] getAllNames() 
	{
		insureInitialized();

		List names = new ArrayList();
		Iterator it = getPackagesSet().iterator();
		while( it.hasNext() ) {
			String pack = (String)it.next();
			names.addAll( 
				removeInnerClassNames( getClassesForPackage( pack ) ) ); 
		}

		if ( nameCompletionIncludesUnqNames )
			names.addAll( getUnqualifiedNameTable().keySet() );

		return (String [])names.toArray(new String[0]);
	}

	
	synchronized void map( URL [] urls ) 
	{ 
		for(int i=0; i< urls.length; i++)
			try{
				map( urls[i] );
			} catch ( IOException e ) {
				String s = "Error constructing classpath: " +urls[i]+": "+e;
				errorWhileMapping( s );
			}
	}

	synchronized void map( URL url ) 
		throws IOException 
	{ 
		String name = url.getFile();
		File f = new File( name );

		if ( f.isDirectory() ) {
			classMapping( "Directory "+ f.toString() );
			map( traverseDirForClasses( f ), new DirClassSource(f) );
		} else if ( isArchiveFileName( name ) ) {
			classMapping("Archive: "+url );
			map( searchJarForClasses( url ), new JarClassSource(url) );
		} 
		
		else {
			String s = "Not a classpath component: "+ name ;
			errorWhileMapping( s );
		}
	}

	private void map( String [] classes, Object source ) {
		for(int i=0; i< classes.length; i++) {
			
			mapClass( classes[i], source );
		}
	}

	private void mapClass( String className, Object source ) 
	{
		
		String [] sa = splitClassname( className );
		String pack = sa[0];
		String clas = sa[1];
		Set set = (Set)packageMap.get( pack );
		if ( set == null ) {
			set = new HashSet();
			packageMap.put( pack, set );
		}
		set.add( className );

		
		Object obj = classSource.get( className );
		
		
		if ( obj == null )
			classSource.put( className, source );
	}

	
	synchronized private void reset() {
		path = new ArrayList();
		compPaths = null;
		clearCachedStructures();
	}

	
	synchronized private void clearCachedStructures() {
		mapsInitialized = false;
		packageMap = new HashMap();
		classSource = new HashMap();
		unqNameTable = null;
		nameSpaceChanged();
	}

	public void classPathChanged() {
		clearCachedStructures();
		notifyListeners();	
	}



	

	static String [] traverseDirForClasses( File dir ) 
		throws IOException	
	{
		List list = traverseDirForClassesAux( dir, dir );
		return (String[])list.toArray( new String[0] );
	}

	static List traverseDirForClassesAux( File topDir, File dir ) 
		throws IOException
	{
		List list = new ArrayList();
		String top = topDir.getAbsolutePath();

		File [] children = dir.listFiles();
		for (int i=0; i< children.length; i++)	{
			File child = children[i];
			if ( child.isDirectory() )
				list.addAll( traverseDirForClassesAux( topDir, child ) );
			else {
				String name = child.getAbsolutePath();
				if ( isClassFileName( name ) ) {
					
					if ( name.startsWith( top ) )
						name = name.substring( top.length()+1 );
					else
						throw new IOException( "problem parsing paths" );

					name = canonicalizeClassName(name);
					list.add( name );
				}
			}
		}
		
		
		return list;
	}

	
	static String [] searchJarForClasses( URL jar ) 
		throws IOException 
	{
		Vector v = new Vector();
		InputStream in = jar.openStream(); 
		ZipInputStream zin = new ZipInputStream(in);

		ZipEntry ze;
		while( (ze= zin.getNextEntry()) != null ) {
			String name=ze.getName();
			if ( isClassFileName( name ) )
				v.addElement( canonicalizeClassName(name) );
		}
		zin.close();

		String [] sa = new String [v.size()];
		v.copyInto(sa);
		return sa;
	}

	public static boolean isClassFileName( String name ){
		return ( name.toLowerCase().endsWith(".class") );
			
	}

	public static boolean isArchiveFileName( String name ){
		name = name.toLowerCase();
		return ( name.endsWith(".jar") || name.endsWith(".zip") );
	}

	
	public static String canonicalizeClassName( String name ) 
	{
		String classname=name.replace('/', '.');
		classname=classname.replace('\\', '.');
		if ( classname.startsWith("class ") )
			classname=classname.substring(6);
		if ( classname.endsWith(".class") )
			classname=classname.substring(0,classname.length()-6);
		return classname;
	}

	
	public static String [] splitClassname ( String classname ) {
		classname = canonicalizeClassName( classname );

		int i=classname.lastIndexOf(".");
		String classn, packn;
		if ( i == -1 )  {
			
			classn = classname;
			packn="<unpackaged>";
		} else {
			packn = classname.substring(0,i);
			classn = classname.substring(i+1);
		}
		return new String [] { packn, classn };
	}

	
	public static Collection removeInnerClassNames( Collection col ) {
		List list = new ArrayList();
		list.addAll(col);
		Iterator it = list.iterator();
		while(it.hasNext()) {
			String name =(String)it.next();
			if (name.indexOf("$") != -1 )
				it.remove();
		}
		return list;
	}
	
	

	static URL [] userClassPathComp;
	public static URL [] getUserClassPathComponents() 
		throws ClassPathException
	{
		if ( userClassPathComp != null )
			return userClassPathComp;

		String cp=System.getProperty("java.class.path");
		String [] paths=StringUtil.split(cp, File.pathSeparator);

		URL [] urls = new URL[ paths.length ];
		try {
			for ( int i=0; i<paths.length; i++)
				
				
				
				urls[i] = new File( 
					new File(paths[i]).getCanonicalPath() ).toURL();
		} catch ( IOException e ) {
			throw new ClassPathException("can't parse class path: "+e);
		}

		userClassPathComp = urls;
		return urls;
	}

	
	public Set getPackagesSet() 
	{
		insureInitialized();
		Set set = new HashSet();
		set.addAll( packageMap.keySet() );

		if ( compPaths != null )
			for (int i=0; i<compPaths.size(); i++)
				set.addAll( 
					((BshClassPath)compPaths.get(i)).packageMap.keySet() );
		return set;
	}

	public void addListener( ClassPathListener l ) {
		listeners.addElement( new WeakReference(l) );
	}
	public void removeListener( ClassPathListener l ) {
		listeners.removeElement( l );
	}

	
	void notifyListeners() {
		for (Enumeration e = listeners.elements(); e.hasMoreElements(); ) {
			WeakReference wr = (WeakReference)e.nextElement();
			ClassPathListener l = (ClassPathListener)wr.get();
			if ( l == null )  
				listeners.removeElement( wr );
			else
				l.classPathChanged();
		}
	}

	static BshClassPath userClassPath;
	
	public static BshClassPath getUserClassPath() 
		throws ClassPathException
	{
		if ( userClassPath == null )
			userClassPath = new BshClassPath( 
				"User Class Path", getUserClassPathComponents() );
		return userClassPath;
	}

	static BshClassPath bootClassPath;
	
	public static BshClassPath getBootClassPath() 
		throws ClassPathException
	{
		if ( bootClassPath == null )
		{
			try 
			{
				
				String rtjar = getRTJarPath();
				URL url = new File( rtjar ).toURL();
				bootClassPath = new BshClassPath( 
					"Boot Class Path", new URL[] { url } );
			} catch ( MalformedURLException e ) {
				throw new ClassPathException(" can't find boot jar: "+e);
			}
		}
		return bootClassPath;
	}


	private static String getRTJarPath()
	{
		String urlString =
			Class.class.getResource("/java/lang/String.class").toExternalForm();

		if ( !urlString.startsWith("jar:file:") )
			return null;

		int i = urlString.indexOf("!");
		if ( i == -1 )
			return null;

		return urlString.substring( "jar:file:".length(), i );
	}

	public abstract static class ClassSource { 
		Object source;
		abstract byte [] getCode( String className );
	}

	public static class JarClassSource extends ClassSource { 
		JarClassSource( URL url ) { source = url; }
		public URL getURL() { return (URL)source; }
		
		public byte [] getCode( String className ) {
			throw new Error("Unimplemented");
		}
		public String toString() { return "Jar: "+source; }
	}

	public static class DirClassSource extends ClassSource 
	{ 
		DirClassSource( File dir ) { source = dir; }
		public File getDir() { return (File)source; }
		public String toString() { return "Dir: "+source; }

		public byte [] getCode( String className ) {
			return readBytesFromFile( getDir(), className );
		}

		public static byte [] readBytesFromFile( File base, String className ) 
		{
			String n = className.replace( '.', File.separatorChar ) + ".class";
			File file = new File( base, n );

			if ( file == null || !file.exists() )
				return null;

			byte [] bytes;
			try {
				FileInputStream fis = new FileInputStream(file);
				DataInputStream dis = new DataInputStream( fis );
		 
				bytes = new byte [ (int)file.length() ];

				dis.readFully( bytes );
				dis.close();
			} catch(IOException ie ) {
				throw new RuntimeException("Couldn't load file: "+file);
			}

			return bytes;
		}

	}

	public static class GeneratedClassSource extends ClassSource 
	{
		GeneratedClassSource( byte [] bytecode ) { source = bytecode; }
		public byte [] getCode( String className ) {
			return (byte [])source; 
		}
	}

	public static void main( String [] args ) throws Exception {
		URL [] urls = new URL [ args.length ];
		for(int i=0; i< args.length; i++)
			urls[i] =  new File(args[i]).toURL();
		BshClassPath bcp = new BshClassPath( "Test", urls );
	}

	public String toString() {
		return "BshClassPath "+name+"("+super.toString()+") path= "+path +"\n"
			+ "compPaths = {" + compPaths +" }";
	}


	
	static class UnqualifiedNameTable extends HashMap {
		void add( String fullname ) {
			String name = splitClassname( fullname )[1];
			Object have = super.get( name );

			if ( have == null )
				super.put( name, fullname );
			else
				if ( have instanceof AmbiguousName )
					((AmbiguousName)have).add( fullname );
				else  
				{
					AmbiguousName an = new AmbiguousName();
					an.add( (String)have );
					an.add( fullname );
					super.put( name, an );
				}
		}
	}

	public static class AmbiguousName {
		List list = new ArrayList();
		public void add( String name ) { 
			list.add( name ); 
		}
		public List get() {
			
			return list;
		}
	}

	
	void nameSpaceChanged() 
	{
		if ( nameSourceListeners == null )
			return;

		for(int i=0; i<nameSourceListeners.size(); i++)
			((NameSource.Listener)(nameSourceListeners.get(i)))
				.nameSourceChanged( this );
	}

	List nameSourceListeners;
	
	public void addNameSourceListener( NameSource.Listener listener ) {
		if ( nameSourceListeners == null )
			nameSourceListeners = new ArrayList();
		nameSourceListeners.add( listener );
	}

	
	static MappingFeedback mappingFeedbackListener;

	
	public static void addMappingFeedback( MappingFeedback mf ) 
	{
		if ( mappingFeedbackListener != null )
			throw new RuntimeException("Unimplemented: already a listener");
		mappingFeedbackListener = mf;
	}

	void startClassMapping() {
		if ( mappingFeedbackListener != null )
			mappingFeedbackListener.startClassMapping();
		else
			System.err.println( "Start ClassPath Mapping" );
	}

	void classMapping( String msg ) {
		if ( mappingFeedbackListener != null ) {
			mappingFeedbackListener.classMapping( msg );
		} else
			System.err.println( "Mapping: "+msg );
	}

	void errorWhileMapping( String s ) {
		if ( mappingFeedbackListener != null )
			mappingFeedbackListener.errorWhileMapping( s );
		else
			System.err.println( s );
	}

	void endClassMapping() {
		if ( mappingFeedbackListener != null )
			mappingFeedbackListener.endClassMapping();
		else
			System.err.println( "End ClassPath Mapping" );
	}
	
	public static interface MappingFeedback
	{
		public void startClassMapping();

		

		
		public void classMapping( String msg );

		public void errorWhileMapping( String msg );

		public void endClassMapping();
	}

}
