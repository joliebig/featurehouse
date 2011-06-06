


package	bsh;

import java.util.*;

import java.io.InputStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;



public class NameSpace 
	implements java.io.Serializable, BshClassManager.Listener, 
	NameSource
{
	public static final NameSpace JAVACODE = 
		new NameSpace("Called from compiled Java code");

	
	

	public String name; 
    private NameSpace parent;
    private Hashtable variables;
    private Hashtable methods;
    private Hashtable importedClasses;
    private This thisReference;
    private Vector importedPackages;

	
	transient private static boolean superImport;

	
    transient private Hashtable classCache;

	

    public NameSpace( String name ) { 
		this( null, name );
	}

    public NameSpace( NameSpace parent, String name ) {
		setName(name);
		setParent(parent);

		
		BshClassManager.addCMListener(this);
    }

	public void setName( String name ) {
		this.name = name;
	}
	public String getName() {
		return this.name;
	}

	SimpleNode callerInfoNode;
	
	void setNode( SimpleNode node ) {
		this.callerInfoNode= node;
	}
	SimpleNode getNode() {
		return this.callerInfoNode;
	}

	
	public Object get( String name, Interpreter interpreter ) 
		throws EvalError 
	{
		CallStack callstack = new CallStack();
		return getNameResolver( name ).toObject( callstack, interpreter );
	}


	
    public void	setVariable(String name, Object	value) throws EvalError 
	{
		if ( variables == null )
			variables =	new Hashtable();

		
		if ( value == null ) {
			variables.remove(name);
			return;
		}

		
		
		boolean recurse = Interpreter.strictJava;
		Object current = getVariableImpl( name, recurse );

		
		if ( (current != null) && (current instanceof TypedVariable) )
		{
			try {
				((TypedVariable)current).setValue(value);
			} catch(EvalError e) {
				throw new EvalError(
					"Typed variable: " + name + ": " + e.getMessage());
			} 
		} else
			if ( Interpreter.strictJava )
				throw new EvalError(
					"(Strict Java mode) Assignment to undeclared variable: "
					+name );
			else
				variables.put(name, value);
    }

	
	public String [] getVariableNames() {
		if ( variables == null )
			return new String [0];
		else
			return enumerationToStringArray( variables.keys() );
	}

	
	public String [] getMethodNames() {
		if ( methods == null )
			return new String [0];
		else
			return enumerationToStringArray( methods.keys() );
	}

	
	public BshMethod [] getMethods() {
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

    public NameSpace getSuper()
    {
		if(parent != null)
			return parent;
		else
			return this;
    }

    public NameSpace getGlobal()
    {
		if(parent != null)
			return parent.getGlobal();
		else
			return this;
    }

	
	
    This getThis( Interpreter declaringInterpreter ) {

		if ( thisReference == null )
			thisReference = This.getThis( this, declaringInterpreter );

		return thisReference;
    }

	
	public void prune() {
		setParent( null );

	
	}

	public void setParent( NameSpace parent ) {
		this.parent = parent;

		
		if ( parent == null )
			loadDefaultImports();
	}

	
    public Object getVariable( String name ) {
		return getVariable( name, true );
	}

	
    public Object getVariable( String name, boolean recurse ) {
		Object val = getVariableImpl( name, recurse );
		return unwrapVariable( val );
    }

	
	protected Object unwrapVariable( Object val ) {
		if (val instanceof TypedVariable)
			val	= ((TypedVariable)val).getValue();

		return (val == null) ? Primitive.VOID :	val;
	}

	
    protected Object getVariableImpl( String name, boolean recurse ) {
		Object val = null;

		if(variables !=	null)
			val	= variables.get(name);

		if ( recurse && (val == null) && (parent != null) )
			val	= parent.getVariableImpl(name, recurse);

		return val;
    }

    
    public void	setTypedVariable(
		String	name, Class type, Object value,	boolean	isFinal) 
		throws EvalError 
	{
		if (variables == null)
			variables =	new Hashtable();

		if (value == null)
		{
			
			if(type.isPrimitive())
			{
			if(type	== Boolean.TYPE)
				value = new	Primitive(Boolean.FALSE);
			else if(type ==	Byte.TYPE)
				value = new	Primitive((byte)0);
			else if(type ==	Short.TYPE)
				value = new	Primitive((short)0);
			else if(type ==	Character.TYPE)
				value = new	Primitive((char)0);
			else if(type ==	Integer.TYPE)
				value = new	Primitive((int)0);
			else if(type ==	Long.TYPE)
				value = new	Primitive(0L);
			else if(type ==	Float.TYPE)
				value = new	Primitive(0.0f);
			else if(type ==	Double.TYPE)
				value = new	Primitive(0.0d);
			}
			else
				value =	Primitive.NULL;
		}

		
		if ( variables.containsKey(name) ) 
		{
			Object existing = getVariableImpl( name, false );
			
			if ( existing instanceof TypedVariable ) 
			{
				
				if ( ((TypedVariable)existing).getType() != type )
					throw new EvalError( "Typed variable: "+name
						+" was previously declared with type: " 
						+ ((TypedVariable)existing).getType() );
				else {
					
					((TypedVariable)existing).setValue( value );
					return;
				}
			}
			
		} 

		
		variables.put(name, new	TypedVariable(type, value, isFinal));
    }

	
    public void	setMethod(String name, BshMethod method) 
	{
		if(methods == null)
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
	{
		BshMethod method = null;

		Object m = null;
		if ( methods != null )
			m = methods.get(name);

		if ( m instanceof Vector ) {
			Vector vm = (Vector)m;
			BshMethod [] ma = new BshMethod[ vm.size() ];
			vm.copyInto( ma );

			Class [][] candidates = new Class[ ma.length ][];
			for( int i=0; i< ma.length; i++ )
				candidates[i] = ma[i].getArgTypes();

			int match = Reflect.findMostSpecificSignature( sig, candidates );
			if ( match != -1 )
				method = ma[match];
		} else
			method = (BshMethod)m;
			
		if ((method == null) && (parent != null))
			return parent.getMethod( name, sig );

		return method;
    }

	
    public void	importClass(String name)
    {
		if(importedClasses == null)
			importedClasses = new Hashtable();

		importedClasses.put(Name.suffix(name, 1), name);
		nameSpaceChanged();
    }

	
    public void	importPackage(String name)
    {
		if(importedPackages == null)
			importedPackages = new Vector();

		importedPackages.addElement(name);
		nameSpaceChanged();
    }

	
    public String[] getImportedPackages()
    {
		Vector v = getImportedPackages(true);
		String[] packages = new	String[ v.size() ];
		v.copyInto(packages);
		return packages;
    }

	
    public Vector getImportedPackages( boolean recurse )
    {
		if ( !recurse )
			return importedPackages;
		else {
			Vector v = new Vector();
			
			if ( parent != null ) {
				String [] psa = parent.getImportedPackages();
				for(int i=0; i<psa.length; i++)
					v.addElement(psa[i]);
			}
			
			if ( importedPackages != null )
				for(int i=0; i< importedPackages.size(); i++)
					v.addElement( importedPackages.elementAt(i) );

			return v;
		}
    }




	
	private void cacheClass( Class c ) {
		if ( classCache == null ) {
			classCache = new Hashtable();
			
		}

		classCache.put(name, c);
	}

	
    public Class getClass( String name)
		throws ClassPathException
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
		throws ClassPathException
    {
		
		boolean unqualifiedName = !Name.isCompound(name);
		Class c = null;

		
		if (classCache != null) {
			c =	(Class)classCache.get(name);

			if ( c != null )
				return c;
		}
			
		
		if ( unqualifiedName ) {
			c = getImportedClassImpl( name );

			
			if ( c != null ) {
				cacheClass( c );
				return c;
			}
		}

		
		c = classForName( name );
		if ( c != null ) {
			
			if ( unqualifiedName )
				cacheClass( c );
			return c;
		}

		
		if ( Interpreter.DEBUG ) 
			Interpreter.debug("getClass(): " + name	+ " not	found in "+this);
		return null;
    }

	
    private Class getImportedClassImpl( String name )
		throws ClassPathException
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
					} catch ( EvalError e ) {  }
				else 
					if ( Interpreter.DEBUG ) Interpreter.debug(
						"imported unpackaged name not found:" +fullname);

				
				if ( clas != null ) {
					
					BshClassManager.cacheClassInfo( fullname, clas );
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

		
		if ( superImport ) 
		{
			BshClassManager bcm = BshClassManager.getClassManager();
			if ( bcm != null ) {
				String s = bcm.getClassNameByUnqName( name );
				if ( s != null )
					return classForName( s );
			}
		}

		return null;
    }

	private Class classForName( String name ) 
	{
		return BshClassManager.classForName( name );
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
	
	
	public static void doSuperImport() 
		throws EvalError
	{
		BshClassManager bcm = BshClassManager.getClassManager();
		if ( bcm != null )
			bcm.doSuperImport();
		superImport = true;
	}

    static class TypedVariable implements java.io.Serializable 
	{
		Class type;
		Object value = null; 
		boolean	isFinal;

		TypedVariable(Class type, Object value,	boolean	isFinal)
			throws EvalError
		{
			this.type =	type;
			if ( type == null )
				throw new InterpreterError("null type in typed var: "+value);
			this.isFinal = isFinal;
			setValue( value );
		}

		
		void setValue(Object val) throws EvalError
		{
			if ( isFinal && value != null )
				throw new EvalError ("Final variable, can't assign");

			
			val = getAssignableForm(val, type);
			
			
			
			
			if ( val instanceof Primitive && ((Primitive)val).isNumber() )
				try {
					val = BSHCastExpression.castPrimitive( 
						(Primitive)val, type );
				} catch ( EvalError e ) {
					throw new InterpreterError("auto assignment cast failed");
				}

			this.value= val;
		}

		Object getValue() { return value; }

		Class getType() { return type;	}

		public String toString() { 
			return "TypedVariable: "+type+", value:"+value;
		}
    }

	
    public static Object checkAssignableFrom(Object rhs, Class lhsType)
		throws EvalError
    {
		return getAssignableForm( rhs, lhsType );
	}

	
    public static Object getAssignableForm( Object rhs, Class lhsType )
		throws EvalError
    {
	
		Class originalType;

		if ( lhsType == null )
			throw new InterpreterError(
				"Null value for type in getAssignableForm");

		if(rhs == null)
			throw new InterpreterError("Null value in getAssignableForm.");

		if(rhs == Primitive.VOID)
			throw new EvalError( "Undefined variable or class name");

		if (rhs == Primitive.NULL)
			if(!lhsType.isPrimitive())
				return rhs;
			else
				throw new EvalError(
					"Can't assign null to primitive type " + lhsType.getName());

		Class rhsType;

		if ( rhs instanceof Primitive ) 
		{
			
			rhsType = originalType = ((Primitive)rhs).getType();

			
			if ( lhsType.isPrimitive() ) {
				
				
			} else
			{
				
				
				
				if( Boolean.class.isAssignableFrom(lhsType) ||
					Character.class.isAssignableFrom(lhsType) ||
					Number.class.isAssignableFrom(lhsType) )
				{
					rhs	= ((Primitive)rhs).getValue();
					
					rhsType = rhs.getClass();
				}
				else
					assignmentError(lhsType, originalType);
			}
		} else 
		{
			
			rhsType = originalType = rhs.getClass();

			
			if ( lhsType.isPrimitive() ) {

				
				

				if (rhsType == Boolean.class)
				{
					rhs	= new Primitive((Boolean)rhs);
					rhsType = Boolean.TYPE;
				}
				else if (rhsType == Character.class)
				{
					rhs	= new Primitive((Character)rhs);
					rhsType = Character.TYPE;
				}
				else if (Number.class.isAssignableFrom(rhsType))
				{
					rhs	= new Primitive((Number)rhs);
					rhsType = ((Primitive)rhs).getType();
				}
				else
					assignmentError(lhsType, originalType);
			}
		}

		
		if ( Reflect.isAssignableFrom(lhsType, rhsType) )
			return rhs;

		
		if(lhsType == Short.class)
			if(rhsType == Byte.class)
				return new Short(((Number)rhs).shortValue());

		if(lhsType == Integer.class) {
			if(rhsType == Byte.class || rhsType == Short.class)
				return new Integer(((Number)rhs).intValue());

			if(rhsType == Character.class)
				return new Integer(((Number)rhs).intValue());
		}

		if(lhsType == Long.class) {
			if(rhsType == Byte.class || rhsType == Short.class ||
				rhsType == Integer.class)
				return new Long(((Number)rhs).longValue());

			if(rhsType == Character.class)
				return new Long(((Number)rhs).longValue());
		}

		if(lhsType == Float.class) {
			if(rhsType == Byte.class || rhsType == Short.class ||
				rhsType == Integer.class ||	rhsType	== Long.class)
				return new Float(((Number)rhs).floatValue());

			if(rhsType == Character.class)
				return new Float(((Number)rhs).floatValue());
		}

		if(lhsType == Double.class) {
			if(rhsType == Byte.class || rhsType == Short.class ||
				rhsType == Integer.class ||	rhsType	== Long.class ||
				rhsType == Float.class)
				return new Double(((Number)rhs).doubleValue());

			if(rhsType == Character.class)
				return new Double(((Number)rhs).doubleValue());
		}

		
		if ( Capabilities.canGenerateInterfaces() && 
			lhsType.isInterface() && ( rhs instanceof bsh.This ) ) 
		{
			return ((bsh.This)rhs).getInterface( lhsType );
		}

		assignmentError(lhsType, originalType);

		return rhs;
    }

    private static void	assignmentError(Class lhs, Class rhs) throws EvalError
    {
		String lhsType = Reflect.normalizeClassName(lhs);
		String rhsType = Reflect.normalizeClassName(rhs);
		throw new EvalError ("Can't assign " + rhsType + " to "	+ lhsType);
    }

	public String toString() {
		return
			"NameSpace: "
			+ ( name==null
				? super.toString()
				: name + " (" + super.toString() +")" );
	}

	
    private synchronized void writeObject(java.io.ObjectOutputStream s)
        throws IOException {

		
		s.defaultWriteObject();
	}

	
	public Object invokeMethod( 
		String methodName, Object [] args, Interpreter interpreter ) 
		throws EvalError
	{
		return invokeMethod( methodName, args, interpreter, null, null );
	}

	
	public Object invokeMethod( 
		String methodName, Object [] args, Interpreter interpreter, 
		CallStack callstack, SimpleNode callerInfo ) 
		throws EvalError
	{
		if ( callstack == null ) {
			callstack = new CallStack();
			callstack.push( this );
		}

		
        BshMethod meth = getMethod( methodName, Reflect.getTypes( args ) );
        if ( meth != null )
           return meth.invokeDeclaredMethod( args, interpreter, callstack, callerInfo );

		
		meth = getMethod( "invoke", new Class [] { null, null } );

		
		if ( meth != null )
			return meth.invokeDeclaredMethod( 
				new Object [] { methodName, args }, interpreter, callstack, callerInfo );

		throw new EvalError( "No locally declared method: " 
			+ methodName + " in namespace: " + this );
	}

	
	public void classLoaderChanged() {
		nameSpaceChanged();
	}

	
	public void nameSpaceChanged() {
		classCache = null;
	}

	
    public void loadDefaultImports()
    {
		
		importClass("bsh.EvalError");
		importPackage("javax.swing.event");
		importPackage("javax.swing");
		importPackage("java.awt.event");
		importPackage("java.awt");
		importPackage("java.net");
		importPackage("java.util");
		importPackage("java.io");
		importPackage("java.lang");

	

    }

	
	Name getNameResolver( String name ) {
		
		return new Name(this,name);
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

	
	public static Class identifierToClass( Name.ClassIdentifier ci ) 
	{
		return ci.getTargetClass();
	}

	
	public void clear() 
	{
		variables = null;
		methods = null;
		importedClasses = null;
		importedPackages = null;
		superImport = false;
		if ( parent == null )
			loadDefaultImports();	
    	classCache = null;
	}
}

