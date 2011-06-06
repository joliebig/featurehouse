


package	bsh;

import java.util.*;

import java.io.InputStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import java.lang.reflect.Method;
import java.lang.reflect.Field;



public class NameSpace 
	implements java.io.Serializable, BshClassManager.Listener, 
	NameSource
{
	public static final NameSpace JAVACODE = 
		new NameSpace((BshClassManager)null, "Called from compiled Java code.");
	static {
		JAVACODE.isMethod = true;
	}

	
	

	
	private String nsName; 
    private NameSpace parent;
    private Hashtable variables;
    private Hashtable methods;

    protected Hashtable importedClasses;
    private Vector importedPackages;
	private Vector importedObjects;
	private Vector importedStatic;
    private Vector importedCommands;
	private String packageName;

	transient private BshClassManager classManager;

	
    private This thisReference;

	
    private Hashtable names;

	
	SimpleNode callerInfoNode;

	
	boolean isMethod;
	
	
	boolean isClass;
	Class classStatic;	
	Object classInstance;

	void setClassStatic( Class clas ) {
		this.classStatic = clas;
		importStatic( clas );
	}
	void setClassInstance( Object instance ) {
		this.classInstance = instance;
		importObject( instance );
	}
	Object getClassInstance()
		throws UtilEvalError
	{
		if ( classInstance != null )
			return classInstance;

		if ( classStatic != null 
			
		)
			throw new UtilEvalError(
				"Can't refer to class instance from static context.");
		else
			throw new InterpreterError( 
				"Can't resolve class instance 'this' in: "+this);
	}


	
    transient private Hashtable classCache;

	

	

	
    public NameSpace( NameSpace parent, String name ) 
	{
		
		this( parent, null, name );
	}

    public NameSpace( BshClassManager classManager, String name ) 
	{
		this( null, classManager, name );
	}

    public NameSpace( 
		NameSpace parent, BshClassManager classManager, String name ) 
	{
		
		
		
			

		setName(name);
		setParent(parent);
		setClassManager( classManager );

		
		if ( classManager != null )
			classManager.addListener(this);
    }

	

	public void setName( String name ) {
		this.nsName = name;
	}

	
	public String getName() {
		return this.nsName;
	}

	
	void setNode( SimpleNode node ) {
		callerInfoNode = node;
	}

	
	SimpleNode getNode() 
	{
		if ( callerInfoNode != null )
			return callerInfoNode;
		if ( parent != null )
			return parent.getNode();
		else
			return null;
	}

	
	public Object get( String name, Interpreter interpreter ) 
		throws UtilEvalError 
	{
		CallStack callstack = new CallStack( this );
		return getNameResolver( name ).toObject( callstack, interpreter );
	}

	public void setVariable(String name, Object value) throws UtilEvalError
        {
                setVariable(name,value,false);
        }

	
    public void	setVariable( String name, Object value, boolean strictJava ) 
		throws UtilEvalError 
	{
		
		boolean recurse = Interpreter.LOCALSCOPING ? strictJava : true;
		setVariable( name, value, strictJava, recurse );
	}

	
    void setLocalVariable( 
		String name, Object value, boolean strictJava ) 
		throws UtilEvalError 
	{
		setVariable( name, value, strictJava, false );
	}

	
    void setVariable( 
		String name, Object value, boolean strictJava, boolean recurse ) 
		throws UtilEvalError 
	{
		if ( variables == null )
			variables =	new Hashtable();

		
		if ( value == null ) {
			
			
			unsetVariable(name);
			return;
		}

		
		Variable existing = getVariableImpl( name, recurse );

		
		if ( existing != null )
		{
			try {
				existing.setValue( value, Variable.ASSIGNMENT );
			} catch ( UtilEvalError e ) {
				throw new UtilEvalError(
					"Variable assignment: " + name + ": " + e.getMessage());
			}
		} else 
		
		{
			if ( strictJava )
				throw new UtilEvalError(
					"(Strict Java mode) Assignment to undeclared variable: "
					+name );

			
			
			
			NameSpace varScope = this;

			varScope.variables.put( 
				name, new Variable( name, value, null ) );

			
			nameSpaceChanged();
    	}
	}

	
	public void unsetVariable( String name )
	{
		variables.remove( name );
		nameSpaceChanged();
	}

	
	public String [] getVariableNames() {
		if ( variables == null )
			return new String [0];
		else
			return enumerationToStringArray( variables.keys() );
	}

	
	public String [] getMethodNames() 
	{
		if ( methods == null )
			return new String [0];
		else
			return enumerationToStringArray( methods.keys() );
	}

	
	public BshMethod [] getMethods() 
	{
		if ( methods == null )
			return new BshMethod [0];
		else
			return flattenMethodCollection( methods.elements() );
	}

	private String [] enumerationToStringArray( Enumeration e ) {
		Vector v = new Vector();
		while ( e.hasMoreElements() )
			v.addElement( e.nextElement() );
		String [] sa = new String [ v.size() ];
		v.copyInto( sa );
		return sa;
	}

	
    private BshMethod [] flattenMethodCollection( Enumeration e ) {
        Vector v = new Vector();
        while ( e.hasMoreElements() ) {
            Object o = e.nextElement();
            if ( o instanceof BshMethod )
                v.addElement( o );
            else {
                Vector ov = (Vector)o;
                for(int i=0; i<ov.size(); i++)
                    v.addElement( ov.elementAt( i ) );
            }
        }
        BshMethod [] bma = new BshMethod [ v.size() ];
        v.copyInto( bma );
        return bma;
    }

	
	public NameSpace getParent() {
		return parent;
	}

	
    public This getSuper( Interpreter declaringInterpreter )
    {
		if ( parent != null )
			return parent.getThis( declaringInterpreter );
		else
			return getThis( declaringInterpreter );
    }

	
    public This getGlobal( Interpreter declaringInterpreter )
    {
		if ( parent != null )
			return parent.getGlobal( declaringInterpreter );
		else
			return getThis( declaringInterpreter );
    }

	
	
	
    This getThis( Interpreter declaringInterpreter ) 
	{
		if ( thisReference == null )
			thisReference = This.getThis( this, declaringInterpreter );

		return thisReference;
    }

	public BshClassManager getClassManager() 
	{
		if ( classManager != null )
			return classManager;
		if ( parent != null && parent != JAVACODE )
			return parent.getClassManager();

System.out.println("experiment: creating class manager");
		classManager = BshClassManager.createClassManager( null );
		
		
		return classManager;
	}

	void setClassManager( BshClassManager classManager ) {
		this.classManager = classManager;
	}

	
	public void prune() 
	{
		
		
		
		

		if ( this.classManager == null )


			setClassManager( 
				BshClassManager.createClassManager( null ) );

		setParent( null );
	}

	public void setParent( NameSpace parent ) 
	{
		this.parent = parent;

		
		if ( parent == null )
			loadDefaultImports();
	}

	
    public Object getVariable( String name ) 
		throws UtilEvalError
	{
		return getVariable( name, true );
	}

	
    public Object getVariable( String name, boolean recurse ) 
		throws UtilEvalError
	{
		Variable var = getVariableImpl( name, recurse );
		return unwrapVariable( var );
    }

	
    protected Variable getVariableImpl( String name, boolean recurse ) 
		throws UtilEvalError
	{
		Variable var = null;

		
		
		if ( var == null && isClass )
			var = getImportedVar( name );

		if ( var == null && variables != null )
			var	= (Variable)variables.get(name);

		
		if ( var == null && !isClass )
			var = getImportedVar( name );

		
		if ( recurse && (var == null) && (parent != null) )
			var	= parent.getVariableImpl( name, recurse );

		return var;
    }
	
	
	public Variable [] getDeclaredVariables() 
	{
		if ( variables == null )
			return new Variable[0];
		Variable [] vars = new Variable [ variables.size() ];
		int i=0;
		for( Enumeration e = variables.elements(); e.hasMoreElements(); )
			vars[i++] = (Variable)e.nextElement();
		return vars;
	}

	
	protected Object unwrapVariable( Variable var ) 
		throws UtilEvalError
	{
		return (var == null) ? Primitive.VOID :	var.getValue();
	}

	
    public void	setTypedVariable(
		String	name, Class type, Object value,	boolean	isFinal )
		throws UtilEvalError 
	{
		Modifiers modifiers = new Modifiers();
		if ( isFinal )
			modifiers.addModifier( Modifiers.FIELD, "final" );
		setTypedVariable( name, type, value, modifiers );
	}

    
    public void	setTypedVariable(
		String	name, Class type, Object value,	Modifiers modifiers )
		throws UtilEvalError 
	{
		

		if ( variables == null )
			variables =	new Hashtable();

		
		Variable existing = getVariableImpl( name, false );


		
		
	

		
		if ( existing != null ) 
		{
			
			if ( existing.getType() != null ) 
			{
				
				
				
				if ( existing.getType() != type )
				{
					throw new UtilEvalError( "Typed variable: "+name
						+" was previously declared with type: " 
						+ existing.getType() );
				} else 
				{
					
					existing.setValue( value, Variable.DECLARATION );
					return;
				}
			}
			
			
		} 

		
		variables.put( name, new Variable( name, type, value, modifiers ) );
    }

	

	
    public void	setMethod( String name, BshMethod method )
		throws UtilEvalError
	{
		

		if ( methods == null )
			methods = new Hashtable();

		Object m = methods.get(name);

		if ( m == null )
			methods.put(name, method);
		else 
		if ( m instanceof BshMethod ) {
			Vector v = new Vector();
			v.addElement( m );
			v.addElement( method );
			methods.put( name, v );
		} else 
			((Vector)m).addElement( method );
    }

	
    public BshMethod getMethod( String name, Class [] sig ) 
		throws UtilEvalError
	{
		return getMethod( name, sig, false );
	}

	
    public BshMethod getMethod( 
		String name, Class [] sig, boolean declaredOnly ) 
		throws UtilEvalError
	{
		BshMethod method = null;

		
		
		if ( method == null && isClass && !declaredOnly )
			method = getImportedMethod( name, sig );

		Object m = null;
		if ( method == null && methods != null )
		{
			m = methods.get(name);

			
			if ( m != null ) 
			{
				
				BshMethod [] ma;
				if ( m instanceof Vector ) 
				{
					Vector vm = (Vector)m;
					ma = new BshMethod[ vm.size() ];
					vm.copyInto( ma );
				} else
					ma = new BshMethod[] { (BshMethod)m };

				
				Class [][] candidates = new Class[ ma.length ][];
				for( int i=0; i< ma.length; i++ )
					candidates[i] = ma[i].getParameterTypes();

				int match = 
					Reflect.findMostSpecificSignature( sig, candidates );
				if ( match != -1 )
					method = ma[match];
			}
		}

		if ( method == null && !isClass && !declaredOnly )
			method = getImportedMethod( name, sig );
		
		
		if ( !declaredOnly && (method == null) && (parent != null) )
			return parent.getMethod( name, sig );

		return method;
    }

	
    public void	importClass(String name)
    {
		if ( importedClasses == null )
			importedClasses = new Hashtable();

		importedClasses.put( Name.suffix(name, 1), name );
		nameSpaceChanged();
    }

	
    public void	importPackage(String name)
    {
		if(importedPackages == null)
			importedPackages = new Vector();

		
		if ( importedPackages.contains( name ) )
			importedPackages.remove( name );

		importedPackages.addElement(name);
		nameSpaceChanged();
    }

    static class CommandPathEntry
	{
		String path;
		Class clas;

		CommandPathEntry(String path, Class clas)
		{
			this.path = path;
			this.clas = clas;
		}
	}

	
	public void addCommandPath(String path, Class clas)
	{
		if(importedCommands == null)
			importedCommands = new Vector();

		if(!path.endsWith("/"))
			path = path + "/";
		importedCommands.addElement(new CommandPathEntry(path,clas));
	}

	
	public void removeCommandPath(String path, Class clas)
	{
		if(importedCommands == null)
			return;

		for(int i = 0; i < importedCommands.size(); i++)
		{
			CommandPathEntry entry = (CommandPathEntry)importedCommands
				.elementAt(i);
			if(entry.path.equals(path) && entry.clas == clas)
			{
				importedCommands.removeElementAt(i);
				return;
			}
		}
	}

	
	public InputStream getCommand(String name)
	{
		if(importedCommands != null)
		{
			String extName = name + ".bsh";
			for(int i = importedCommands.size() - 1; i >= 0; i--)
			{
				CommandPathEntry entry = (CommandPathEntry)importedCommands
					.elementAt(i);
				InputStream in = entry.clas.getResourceAsStream(entry.path + extName);
				if(in != null)
					return in;
			}
		}

		if(parent == null)
			return null;
		else
			return parent.getCommand(name);
	}

	
	public Object getCommand( 	
		String name, Class [] argTypes, Interpreter interpreter ) 
		throws UtilEvalError
	{
		if (Interpreter.DEBUG) Interpreter.debug("getCommand: "+name);
		BshClassManager bcm = interpreter.getClassManager();

		InputStream in = getCommand( name );

		if ( in != null )
			return loadScriptedCommand( 
				in, name, argTypes, name, interpreter );

		

		if ( parent != null )
			return parent.getCommand( name, argTypes, interpreter );
		else
			return null;
	}

	protected BshMethod getImportedMethod( String name, Class [] sig ) 
		throws UtilEvalError
	{
		
		if ( importedObjects != null )
		for(int i=0; i<importedObjects.size(); i++)
		{
			Object object = importedObjects.elementAt(i);
			Class clas = object.getClass();
			Method method = Reflect.resolveJavaMethod( 
				getClassManager(), clas, name, sig, false );
			if ( method != null )
				return new BshMethod( method, object );
		}

		
		if ( importedStatic!= null )
		for(int i=0; i<importedStatic.size(); i++)
		{
			Class clas = (Class)importedStatic.elementAt(i);
			Method method = Reflect.resolveJavaMethod( 
				getClassManager(), clas, name, sig, true );
			if ( method != null )
				return new BshMethod( method, null );
		}

		return null;
	}

	protected Variable getImportedVar( String name ) 
		throws UtilEvalError
	{
		
		if ( importedObjects != null )
		for(int i=0; i<importedObjects.size(); i++)
		{
			Object object = importedObjects.elementAt(i);
			Class clas = object.getClass();
			Field field = Reflect.resolveJavaField( 
				clas, name, false );
			if ( field != null )
				return new Variable( 
					name, field.getType(), new LHS( object, field ) );
		}

		
		if ( importedStatic!= null )
		for(int i=0; i<importedStatic.size(); i++)
		{
			Class clas = (Class)importedStatic.elementAt(i);
			Field field = Reflect.resolveJavaField( 
				clas, name, true );
			if ( field != null )
				return new Variable( name, field.getType(), new LHS( field ) );
		}

		return null;
	}

	
	
	private BshMethod loadScriptedCommand( 
		InputStream in, String name, Class [] argTypes, String resourcePath, 
		Interpreter interpreter )
		throws UtilEvalError
	{
		try {
			interpreter.eval( 
				new InputStreamReader(in), this, resourcePath );
		} catch ( EvalError e ) {
		
			Interpreter.debug( e.toString() );
			throw new UtilEvalError( 
				"Error loading script: "+ e.getMessage());
		}

		
		BshMethod meth = getMethod( name, argTypes );
		

		return meth;
	}

	
	void cacheClass( String name, Class c ) {
		if ( classCache == null ) {
			classCache = new Hashtable();
			
		}

		classCache.put(name, c);
	}

	
    public Class getClass( String name )
		throws UtilEvalError
    {
		Class c = getClassImpl(name);
		if ( c != null )
			return c;
		else
			
			if ( parent != null )
				return parent.getClass( name );
			else
				return null;
	}

	
    private Class getClassImpl( String name )
		throws UtilEvalError
    {
		Class c = null;

		
		if (classCache != null) {
			c =	(Class)classCache.get(name);

			if ( c != null )
				return c;
		}

		
		boolean unqualifiedName = !Name.isCompound(name);

		
		if ( unqualifiedName ) 
		{
			
			if ( c == null )
				c = getImportedClassImpl( name );

			
			if ( c != null ) {
				cacheClass( name, c );
				return c;
			}
		}

		
		c = classForName( name );
		if ( c != null ) {
			
			if ( unqualifiedName )
				cacheClass( name, c );
			return c;
		}

		
		if ( Interpreter.DEBUG ) 
			Interpreter.debug("getClass(): " + name	+ " not	found in "+this);
		return null;
    }

	
    private Class getImportedClassImpl( String name )
		throws UtilEvalError
    {
		
		String fullname = null;
		if ( importedClasses != null )
			fullname = (String)importedClasses.get(name);
		
		
		

		if ( fullname != null ) 
		{
			
			
			Class clas=classForName(fullname);
			
			
			if ( clas == null ) 
			{
				
				
				

				if ( Name.isCompound( fullname ) )
					try {
						clas = getNameResolver( fullname ).toClass();
					} catch ( ClassNotFoundException e ) {  }
				else 
					if ( Interpreter.DEBUG ) Interpreter.debug(
						"imported unpackaged name not found:" +fullname);

				
				if ( clas != null ) {
					
					getClassManager().cacheClassInfo( fullname, clas );
					return clas;
				}
			} else
				return clas;

			
			
			return null;  
		}

		
		if ( importedPackages != null )
			for(int i=importedPackages.size()-1; i>=0; i--)
			{
				String s = ((String)importedPackages.elementAt(i)) + "." + name;
				Class c=classForName(s);
				if ( c != null )
					return c;
			}

		BshClassManager bcm = getClassManager();
		
		if ( bcm.hasSuperImport() ) 
		{
			String s = bcm.getClassNameByUnqName( name );
			if ( s != null )
				return classForName( s );
		}

		return null;
    }

	private Class classForName( String name ) 
	{
		return getClassManager().classForName( name );
	}

	
	public String [] getAllNames() 
	{
		Vector vec = new Vector();
		getAllNamesAux( vec );
		String [] names = new String [ vec.size() ];
		vec.copyInto( names );
		return names;
	}

	
	protected void getAllNamesAux( Vector vec ) 
	{
		Enumeration varNames = variables.keys();
		while( varNames.hasMoreElements() )
			vec.addElement( varNames.nextElement() );

		Enumeration methodNames = methods.keys();
		while( methodNames.hasMoreElements() )
			vec.addElement( methodNames.nextElement() );

		if ( parent != null )
			parent.getAllNamesAux( vec );
	}

	Vector nameSourceListeners;
	
	public void addNameSourceListener( NameSource.Listener listener ) {
		if ( nameSourceListeners == null )
			nameSourceListeners = new Vector();
		nameSourceListeners.addElement( listener );
	}
	
	
	public void doSuperImport() 
		throws UtilEvalError
	{
		getClassManager().doSuperImport();
	}


	public String toString() {
		return "NameSpace: " 
			+ ( nsName==null
				? super.toString()
				: nsName + " (" + super.toString() +")" )
			+ ( isClass ? " (isClass) " : "" )
			+ ( isMethod ? " (method) " : "" )
			+ ( classStatic != null ? " (class static) " : "" )
			+ ( classInstance != null ? " (class instance) " : "" );
	}

	
    private synchronized void writeObject(java.io.ObjectOutputStream s)
        throws IOException 
	{
		
		names = null;
	
		s.defaultWriteObject();
	}

	
	public Object invokeMethod( 
		String methodName, Object [] args, Interpreter interpreter ) 
		throws EvalError
	{
		return invokeMethod( 
			methodName, args, interpreter, null, null );
	}

	
	public Object invokeMethod( 
		String methodName, Object [] args, Interpreter interpreter, 
		CallStack callstack, SimpleNode callerInfo ) 
		throws EvalError
	{
		return getThis( interpreter ).invokeMethod( 
			methodName, args, interpreter, callstack, callerInfo,
			false );
	}

	
	public void classLoaderChanged() {
		nameSpaceChanged();
	}

	
	public void nameSpaceChanged() {
		classCache = null;
		names = null;
	}

	
    public void loadDefaultImports()
    {
		
		importClass("bsh.EvalError");
		importClass("bsh.Interpreter");
		importPackage("javax.swing.event");
		importPackage("javax.swing");
		importPackage("java.awt.event");
		importPackage("java.awt");
		importPackage("java.net");
		importPackage("java.util");
		importPackage("java.io");
		importPackage("java.lang");
		addCommandPath("/bsh/commands",getClass());
    }

	
	Name getNameResolver( String ambigname ) 
	{
		if ( names == null )
			names = new Hashtable();

		Name name = (Name)names.get( ambigname );

		if ( name == null ) {
			name = new Name( this, ambigname );
			names.put( ambigname, name );
		} 

		return name;
	}

	public int getInvocationLine() {
		SimpleNode node = getNode();
		if ( node != null )
			return node.getLineNumber();
		else
			return -1;
	}
	public String getInvocationText() {
		SimpleNode node = getNode();
		if ( node != null )
			return node.getText();
		else
			return "<invoked from Java code>";
	}

	
	public static Class identifierToClass( ClassIdentifier ci ) 
	{
		return ci.getTargetClass();
	}


	
	public void clear() 
	{
		variables = null;
		methods = null;
		importedClasses = null;
		importedPackages = null;
		importedCommands = null;
		importedObjects = null;
		if ( parent == null )
			loadDefaultImports();	
    	classCache = null;
		names = null;
	}

	
	
	public void importObject( Object obj ) 
	{
		if ( importedObjects == null )
			importedObjects = new Vector();

		
		if ( importedObjects.contains( obj ) )
			importedObjects.remove( obj );

		importedObjects.addElement( obj );
		nameSpaceChanged();

	}

	
	public void importStatic( Class clas ) 
	{
		if ( importedStatic == null )
			importedStatic = new Vector();

		
		if ( importedStatic.contains( clas ) )
			importedStatic.remove( clas );

		importedStatic.addElement( clas );
		nameSpaceChanged();
	}

	
	void setPackage( String packageName ) 
	{
		this.packageName = packageName;
	}

	String getPackage() 
	{
		if ( packageName != null )
			return packageName;

		if ( parent != null )
			return parent.getPackage();
		
		return null;
	}
}

