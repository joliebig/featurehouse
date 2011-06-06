

package bsh;

import java.lang.reflect.*;
import java.io.*;
import java.util.Vector;



class Reflect 
{
    
    public static Object invokeObjectMethod(
		Object object, String methodName, Object[] args, 
		Interpreter interpreter, CallStack callstack, SimpleNode callerInfo ) 
		throws ReflectError, EvalError, InvocationTargetException
	{
		
		if ( object instanceof This && !This.isExposedThisMethod( methodName) ) 
			return ((This)object).invokeMethod( 
				methodName, args, interpreter, callstack, callerInfo,
				false );
		else 
		
		{ 
			
			try {
				BshClassManager bcm = 
					interpreter == null ? null : interpreter.getClassManager();
				Class clas = object.getClass();

				Method method = resolveExpectedJavaMethod( 
					bcm, clas, object, methodName, args, false );

				return invokeOnMethod( method, object, args );
			} catch ( UtilEvalError e ) {
				throw e.toEvalError( callerInfo, callstack );
			}
		}
    }

    
    public static Object invokeStaticMethod(
		BshClassManager bcm, Class clas, String methodName, Object [] args )
        throws ReflectError, UtilEvalError, InvocationTargetException
    {
        Interpreter.debug("invoke static Method");
        Method method = resolveExpectedJavaMethod( 
			bcm, clas, null, methodName, args, true );
		return invokeOnMethod( method, null, args );
    }

	
	static Object invokeOnMethod( 
		Method method, Object object, Object[] args ) 
		throws ReflectError, InvocationTargetException
	{
		if ( args == null )
			args = new Object[0];

		if ( Interpreter.DEBUG ) 
		{
			Interpreter.debug("Invoking method (entry): "
				+method+" with args:" );
			for(int i=0; i<args.length; i++)
				Interpreter.debug(
					"args["+i+"] = "+args[i]
					+" type = "+args[i].getClass() );
		}
		
		
		Object [] tmpArgs = new Object [ args.length ];
		Class [] types = method.getParameterTypes();
		try {
			for (int i=0; i<args.length; i++)
				tmpArgs[i] = Types.getAssignableForm( args[i], types[i] );
		} catch ( UtilEvalError e ) {
			throw new InterpreterError(
				"illegal argument type in method invocation: "+e );
		}

		
		tmpArgs = Primitive.unwrap( tmpArgs );

		if ( Interpreter.DEBUG ) 
		{
			Interpreter.debug("Invoking method (after massaging values): "
				+method+" with tmpArgs:" );
			for(int i=0; i<tmpArgs.length; i++)
				Interpreter.debug(
					"tmpArgs["+i+"] = "+tmpArgs[i]
					+" type = "+tmpArgs[i].getClass() );
		}

		try {
			Object returnValue = method.invoke( object, tmpArgs );
			if ( returnValue == null )
				returnValue = Primitive.NULL;
			Class returnType = method.getReturnType();

			return Primitive.wrap( returnValue, returnType );
		} catch( IllegalAccessException e ) {
			throw new ReflectError( "Cannot access method " 
				+ StringUtil.methodString(
					method.getName(), method.getParameterTypes() ) 
				+ " in '" + method.getDeclaringClass() + "' :" + e );
		}
	}

    public static Object getIndex(Object array, int index)
        throws ReflectError, UtilTargetError
    {
		if ( Interpreter.DEBUG ) 
			Interpreter.debug("getIndex: "+array+", index="+index);
        try {
            Object val = Array.get(array, index);
            return Primitive.wrap( val, array.getClass().getComponentType() );
        }
        catch( ArrayIndexOutOfBoundsException  e1 ) {
			throw new UtilTargetError( e1 );
        } catch(Exception e) {
            throw new ReflectError("Array access:" + e);
        }
    }

    public static void setIndex(Object array, int index, Object val)
        throws ReflectError, UtilTargetError
    {
        try {
            val = Primitive.unwrap(val);
            Array.set(array, index, val);
        }
        catch( ArrayStoreException e2 ) {
			throw new UtilTargetError( e2 );
        } catch( IllegalArgumentException e1 ) {
			throw new UtilTargetError( 
				new ArrayStoreException( e1.toString() ) );
        } catch(Exception e) {
            throw new ReflectError("Array access:" + e);
        }
    }

    public static Object getStaticField(Class clas, String fieldName)
        throws UtilEvalError, ReflectError
    {
        return getFieldValue( clas, null, fieldName, true);
    }

	
    public static Object getObjectField( Object object, String fieldName )
        throws UtilEvalError, ReflectError
    {
		if ( object instanceof This )
			return ((This)object).namespace.getVariable( fieldName );
		else {
			try {
				return getFieldValue(
					object.getClass(), object, fieldName, false);
			} catch ( ReflectError e ) {
				

				if ( hasObjectPropertyGetter( object.getClass(), fieldName ) )
					return getObjectProperty( object, fieldName );
				else
					throw e;
			}
		}
    }

    static LHS getLHSStaticField(Class clas, String fieldName)
        throws UtilEvalError, ReflectError
    {
        Field f = resolveExpectedJavaField( 
			clas, fieldName, true);
        return new LHS(f);
    }

	
    static LHS getLHSObjectField( Object object, String fieldName )
        throws UtilEvalError, ReflectError
    {
		if ( object instanceof This )
		{
			
			
			boolean recurse = false; 
			return new LHS( ((This)object).namespace, fieldName, recurse );
		}

		try {
			Field f = resolveExpectedJavaField( 
				object.getClass(), fieldName, false );
			return new LHS(object, f);
		} catch ( ReflectError e ) 
		{
			
			if ( hasObjectPropertySetter( object.getClass(), fieldName ) )
				return new LHS( object, fieldName );
			else
				throw e;
		}
    }

    private static Object getFieldValue(
		Class clas, Object object, String fieldName, boolean onlyStatic ) 
		throws UtilEvalError, ReflectError
    {
        try {
            Field f = resolveExpectedJavaField( clas, fieldName, onlyStatic );

            Object value = f.get(object);
            Class returnType = f.getType();
            return Primitive.wrap( value, returnType );

        } catch( NullPointerException e ) { 
            throw new ReflectError(
				"???" + fieldName + " is not a static field.");
        } catch(IllegalAccessException e) {
            throw new ReflectError("Can't access field: " + fieldName);
        }
    }

	
	
    protected static Field resolveJavaField( 
		Class clas, String fieldName, boolean onlyStatic )
        throws UtilEvalError
    {
		try {
			return resolveExpectedJavaField( clas, fieldName, onlyStatic );
		} catch ( ReflectError e ) { 
			return null;
		}
	}

	
	
    protected static Field resolveExpectedJavaField( 
		Class clas, String fieldName, boolean onlyStatic
	)
        throws UtilEvalError, ReflectError
    {
		Field f;
        try {
			if ( Capabilities.haveAccessibility() )
				f = findAccessibleField( clas, fieldName );
			else
				
				f = clas.getField(fieldName);
        }
        catch( NoSuchFieldException e)
        {
			
            throw new ReflectError("No such field: " + fieldName );
        }

		if ( onlyStatic && !Modifier.isStatic( f.getModifiers() ) )
			throw new UtilEvalError(
				"Can't reach instance field: "+fieldName
				+" from static context: "+clas.getName() );

		return f;
    }

	
	
	private static Field findAccessibleField( Class clas, String fieldName ) 
		throws UtilEvalError, NoSuchFieldException
	{
		Field field;

		
		try {
			field = clas.getField(fieldName);
			ReflectManager.RMSetAccessible( field );
			return field;
		} catch ( NoSuchFieldException e ) { }

		
		while ( clas != null )
		{
			try {
				field = clas.getDeclaredField(fieldName);
				ReflectManager.RMSetAccessible( field );
				return field;

				

			} catch(NoSuchFieldException e) { }

			clas = clas.getSuperclass();
		}
		throw new NoSuchFieldException( fieldName );
	}

	
    protected static Method resolveExpectedJavaMethod(
		BshClassManager bcm, Class clas, Object object, 
		String name, Object[] args, boolean onlyStatic )
        throws ReflectError, UtilEvalError
    {
		Method method = resolveJavaMethod( 
			bcm, clas, object, name, args, onlyStatic );

		if ( method != null )
			return method;

		Class [] types = Types.getTypes(args);
		throw new ReflectError(
			( onlyStatic ? "Static method " : "Method " )
			+ StringUtil.methodString(name, types) + 
			" not found in class'" + clas.getName() + "'");
	}

    
	
    protected static Method resolveJavaMethod(
		BshClassManager bcm, Class clas, Object object, 
		String name, Object[] args, boolean onlyStatic )
        throws UtilEvalError
    {
		
		if ( object == Primitive.NULL )
			throw new UtilTargetError( new NullPointerException(
				"Attempt to invoke method " +name+" on null value" ) );

        Class [] types = Types.getTypes(args);
		return resolveJavaMethod( bcm, clas, name, types, onlyStatic );
	}

	
    protected static Method resolveJavaMethod(
		BshClassManager bcm, Class clas, String name, 
		Class [] types, boolean onlyStatic )
        throws UtilEvalError
    {
		if ( clas == null )
			throw new InterpreterError("null class");

		Method method = null;
		if ( bcm == null ) 
			Interpreter.debug("resolveJavaMethod UNOPTIMIZED lookup");
		else {
			method = bcm.getResolvedMethod( clas, name, types, onlyStatic );
			if ( method != null )
				return method;
		}

		if ( Interpreter.DEBUG )
			Interpreter.debug( "Searching for method: "+
				StringUtil.methodString(name, types)
					+ " in '" + clas.getName() + "'" );

		
		try {
			method  = findAccessibleMethod( clas, name, types );
		} catch ( SecurityException e ) { }

		
		
		
		if ( method == null && types.length > 0 ) 
		{
			
			Vector mv = new Vector();
			Class c = clas;
			while( c != null )
			{
				Method [] m = c.getDeclaredMethods();
				for(int i=0; i<m.length; i++)
					mv.add( m[i] );
				c = c.getSuperclass();
			}
			Method [] methods = new Method [mv.size()];
			mv.copyInto( methods );

			boolean publicOnly = !Capabilities.haveAccessibility();
			method = findMostSpecificMethod( name, types, methods, publicOnly );

			if ( method != null && !Modifier.isPublic( method.getModifiers() ) )
			{
				try {
					ReflectManager.RMSetAccessible( method );
				} catch ( UtilEvalError e ) {  }
			}


		}

		if ( method != null 
			&& onlyStatic && !Modifier.isStatic( method.getModifiers() ) 
		)
			throw new UtilEvalError(
				"Cannot reach instance method: "
				+ StringUtil.methodString(
					method.getName(), method.getParameterTypes() )
				+ " from static context: "+ clas.getName() );

		
		if ( method != null && bcm != null )
			bcm.cacheResolvedMethod( clas, types, method );

		return method;
	}

	
	
	static Method findAccessibleMethod( 
		Class clas, String name, Class [] types ) 
		throws UtilEvalError
	{
		Method meth = null;
		Method inaccessibleVersion = null;
		Vector classQ = new Vector();

		classQ.addElement( clas );
		Method found = null;
		while ( classQ.size() > 0 ) 
		{
			Class c = (Class)classQ.firstElement();
			classQ.removeElementAt(0);

			
			
			if ( Modifier.isPublic( c.getModifiers() )
				|| ( Capabilities.haveAccessibility() ) )
			{
				try 
				{
					meth = c.getDeclaredMethod( name, types );

					
					if ( ( Modifier.isPublic( meth.getModifiers() )
						&& Modifier.isPublic( c.getModifiers() ) )
						|| ( Capabilities.haveAccessibility() 
							&& ReflectManager.RMSetAccessible( meth ) ) )
					{
						found = meth; 
						break;
					}
					else
					{
						
						inaccessibleVersion = meth;
					}
				} catch ( NoSuchMethodException e ) { 
					
				}
			}
			
			
			
			if ( !c.isInterface() ) {
				Class superclass = c.getSuperclass();
				if ( superclass != null )
					classQ.addElement((Object)superclass);
			}

			
			Class [] intfs = c.getInterfaces();
			for( int i=0; i< intfs.length; i++ )
				classQ.addElement((Object)intfs[i]);
		}

		if ( found != null )
			return found;

		if ( inaccessibleVersion != null )
			throw new UtilEvalError("Found non-public method: "
				+inaccessibleVersion
				+".  Use setAccessibility(true) to enable access to "
				+" private and protected members of classes." );
		
		return null; 
	}

	
    static Object constructObject( Class clas, Object[] args )
        throws ReflectError, InvocationTargetException
    {
		if ( clas.isInterface() )
			throw new ReflectError(
				"Can't create instance of an interface: "+clas);

        Object obj = null;
        Class[] types = Types.getTypes(args);
        Constructor con = null;

		
		Constructor[] constructors = clas.getDeclaredConstructors();
		if ( Interpreter.DEBUG ) 
			Interpreter.debug("Looking for most specific constructor: "+clas);
		con = findMostSpecificConstructor(types, constructors);

		if ( con == null )
		{
			if ( types.length == 0 )
				throw new ReflectError(
					"Can't find default constructor for: "+clas);
			else
				throw new ReflectError(
					"Can't find constructor: " 
					+ StringUtil.methodString( clas.getName(), types )
					+" in class: "+ clas.getName() );
		}

		if ( !Modifier.isPublic( con.getModifiers() )
			&& Capabilities.haveAccessibility() )
			try {
				ReflectManager.RMSetAccessible( con );
			} catch ( UtilEvalError e ) {  }

        args=Primitive.unwrap( args );
        try {
            obj = con.newInstance( args );
        } catch(InstantiationException e) {
            throw new ReflectError("the class is abstract ");
        } catch(IllegalAccessException e) {
            throw new ReflectError(
				"We don't have permission to create an instance."
				+"Use setAccessibility(true) to enable access." );
        } catch(IllegalArgumentException e) {
            throw new ReflectError("the number of arguments was wrong");
        } 
		if (obj == null)
            throw new ReflectError("couldn't construct the object");

        return obj;
    }

    
	
    static Method findMostSpecificMethod(
		String name, Class[] idealMatch, Method[] methods,
		boolean publicOnly )
    {
		
		Vector sigs = new Vector();
		Vector meths = new Vector();
		for(int i=0; i<methods.length; i++)
		{
			if ( publicOnly && !Modifier.isPublic( methods[i].getModifiers() ) )
				continue;

			
			if ( methods[i].getName().equals( name ) ) 
			{
				meths.addElement( methods[i] );
				sigs.addElement( methods[i].getParameterTypes() );
			}
		}

		Class [][] candidates = new Class [ sigs.size() ][];
		sigs.copyInto( candidates );

		if ( Interpreter.DEBUG ) 
			Interpreter.debug("Looking for most specific method: "+name);
		int match = findMostSpecificSignature( idealMatch, candidates );
		if ( match == -1 )
			return null;
		else
			return (Method)meths.elementAt( match );
    }

    
    static Constructor findMostSpecificConstructor(
		Class[] idealMatch, Constructor[] constructors)
    {
		int match = 
			findMostSpecificConstructorIndex( idealMatch, constructors );
		if ( match == -1 )
			return null;
		else
			return constructors[ match ];
    }

    static int findMostSpecificConstructorIndex(
		Class[] idealMatch, Constructor[] constructors)
    {
		Class [][] candidates = new Class [ constructors.length ] [];
		for(int i=0; i< candidates.length; i++ )
			candidates[i] = constructors[i].getParameterTypes();

		return findMostSpecificSignature( idealMatch, candidates );
    }

	
	static int findMostSpecificSignature(
		Class [] idealMatch, Class [][] candidates )
	{
		Class [] bestMatch = null;
		int bestMatchIndex = -1;

		for (int i=0; i < candidates.length; i++) {
			Class[] targetMatch = candidates[i];

            
			if ( Types.isSignatureAssignable(idealMatch, targetMatch ) &&
				((bestMatch == null) ||
					Types.isSignatureAssignable( targetMatch, bestMatch )))
			{
				bestMatch = targetMatch;
				bestMatchIndex = i;
			}
		}

		if ( bestMatch != null )
			return bestMatchIndex;
		else
			return -1;
	}

	private static String accessorName( String getorset, String propName ) {
        return getorset 
			+ String.valueOf(Character.toUpperCase(propName.charAt(0))) 
			+ propName.substring(1);
	}

    public static boolean hasObjectPropertyGetter( 
		Class clas, String propName ) 
	{
		String getterName = accessorName("get", propName );
		try {
			clas.getMethod( getterName, new Class [0] );
			return true;
		} catch ( NoSuchMethodException e ) {  }
		getterName = accessorName("is", propName );
		try {
			Method m = clas.getMethod( getterName, new Class [0] );
			return ( m.getReturnType() == Boolean.TYPE );
		} catch ( NoSuchMethodException e ) {
			return false;
		}
	}

    public static boolean hasObjectPropertySetter( 
		Class clas, String propName ) 
	{
		String setterName = accessorName("set", propName );
		Class [] sig = new Class [] { clas };
		Method [] methods = clas.getMethods();

		
		
		for(int i=0; i<methods.length; i++)
			if ( methods[i].getName().equals( setterName ) )
				return true;
		return false;
	}

    public static Object getObjectProperty(
		Object obj, String propName )
        throws UtilEvalError, ReflectError
    {
        Object[] args = new Object[] { };

        Interpreter.debug("property access: ");
		Method method = null;

		Exception e1=null, e2=null;
		try {
			String accessorName = accessorName( "get", propName );
			method = resolveExpectedJavaMethod( 
				null, obj.getClass(), obj, accessorName, args, false );
		} catch ( Exception e ) { 
			e1 = e;
		}
		if ( method == null )
			try {
				String accessorName = accessorName( "is", propName );
				method = resolveExpectedJavaMethod( 
					null, obj.getClass(), obj, 
					accessorName, args, false );
				if ( method.getReturnType() != Boolean.TYPE )
					method = null;
			} catch ( Exception e ) { 
				e2 = e;
			}
		if ( method == null )
			throw new ReflectError("Error in property getter: "
				+e1 + (e2!=null?" : "+e2:"") );

        try {
			return invokeOnMethod( method, obj, args );
        }
        catch(InvocationTargetException e)
        {
            throw new UtilEvalError("Property accessor threw exception: "
				+e.getTargetException() );
        }
    }

    public static void setObjectProperty(
		Object obj, String propName, Object value)
        throws ReflectError, UtilEvalError
    {
        String accessorName = accessorName( "set", propName );
        Object[] args = new Object[] { value };

        Interpreter.debug("property access: ");
        try {
			Method method = resolveExpectedJavaMethod( 
				null, obj.getClass(), obj, accessorName, args, false );
			invokeOnMethod( method, obj, args );
        }
        catch ( InvocationTargetException e )
        {
            throw new UtilEvalError("Property accessor threw exception: "
				+e.getTargetException() );
        }
    }

    
    public static String normalizeClassName(Class type)
    {
        if ( !type.isArray() )
            return type.getName();

        StringBuffer className = new StringBuffer();
        try {
            className.append( getArrayBaseType(type).getName() +" ");
            for(int i = 0; i < getArrayDimensions(type); i++)
                className.append("[]");
        } catch( ReflectError e ) {  }

        return className.toString();
    }

	
    public static int getArrayDimensions(Class arrayClass)
    {
        if ( !arrayClass.isArray() )
            return 0;

        return arrayClass.getName().lastIndexOf('[') + 1;  
    }

    
    public static Class getArrayBaseType(Class arrayClass) throws ReflectError
    {
        if ( !arrayClass.isArray() )
            throw new ReflectError("The class is not an array.");

		return arrayClass.getComponentType();

    }

	
	public static Object invokeCompiledCommand( 
		Class commandClass, Object [] args, Interpreter interpreter, 
		CallStack callstack )
		throws UtilEvalError
	{
        
        Object[] invokeArgs = new Object[args.length + 2];
        invokeArgs[0] = interpreter;
        invokeArgs[1] = callstack;
        System.arraycopy( args, 0, invokeArgs, 2, args.length );
		BshClassManager bcm = interpreter.getClassManager();
		try {
        	return Reflect.invokeStaticMethod( 
				bcm, commandClass, "invoke", invokeArgs );
		} catch ( InvocationTargetException e ) {
			throw new UtilEvalError(
				"Error in compiled command: "+e.getTargetException() );
		} catch ( ReflectError e ) {
			throw new UtilEvalError("Error invoking compiled command: "+e );
		}
	}

}

