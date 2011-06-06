


package org.gjt.sp.jedit.bsh;

import java.lang.reflect.Field;


class LHS implements ParserConstants, java.io.Serializable
{
	NameSpace nameSpace;
	
	boolean localVar;

	
	static final int
		VARIABLE = 0,
		FIELD = 1,
		PROPERTY = 2,
		INDEX = 3,
		METHOD_EVAL = 4;

	int type;

	String varName;
	String propName;
	Field field;
	Object object;
	int index;


	LHS( NameSpace nameSpace, String varName )
	{
throw new Error("namespace lhs");

	}

	
	LHS( NameSpace nameSpace, String varName, boolean localVar )
	{
		type = VARIABLE;
		this.localVar = localVar;
		this.varName = varName;
		this.nameSpace = nameSpace;
	}

	
	LHS( Field field )
	{
		type = FIELD;
		this.object = null;
		this.field = field;
	}

	
	LHS( Object object, Field field )
	{
		if ( object == null)
			throw new NullPointerException("constructed empty LHS");

		type = FIELD;
		this.object = object;
		this.field = field;
	}

	
	LHS( Object object, String propName )
	{
		if(object == null)
			throw new NullPointerException("constructed empty LHS");

		type = PROPERTY;
		this.object = object;
		this.propName = propName;
	}

	
	LHS( Object array, int index )
	{
		if(array == null)
			throw new NullPointerException("constructed empty LHS");

		type = INDEX;
		this.object = array;
		this.index = index;
	}

	public Object getValue() throws UtilEvalError
	{
		if ( type == VARIABLE )
			return nameSpace.getVariable( varName );

		if (type == FIELD)
			try {
				Object o = field.get( object );
				return Primitive.wrap( o, field.getType() );
			} catch(IllegalAccessException e2) {
				throw new UtilEvalError("Can't read field: " + field);
			}

		if ( type == PROPERTY )
			try {
				return Reflect.getObjectProperty(object, propName);
			}
			catch(ReflectError e) {
				Interpreter.debug(e.getMessage());
				throw new UtilEvalError("No such property: " + propName);
			}

		if ( type == INDEX )
			try {
				return Reflect.getIndex(object, index);
			}
			catch(Exception e) {
				throw new UtilEvalError("Array access: " + e);
			}

		throw new InterpreterError("LHS type");
	}

	
	public Object assign( Object val, boolean strictJava ) 
		throws UtilEvalError
	{
		if ( type == VARIABLE )
		{
			
			if ( localVar )
				nameSpace.setLocalVariable( varName, val, strictJava );
			else
				nameSpace.setVariable( varName, val, strictJava );
		} else 
		if ( type == FIELD )
		{
			try {
				Object fieldVal = val instanceof Primitive ?  
					((Primitive)val).getValue() : val;

				
				ReflectManager.RMSetAccessible( field );
				field.set( object, fieldVal );
				return val;
			}
			catch( NullPointerException e) {   
    			throw new UtilEvalError(
					"LHS ("+field.getName()+") not a static field.");
			}     
   			catch( IllegalAccessException e2) {   
				throw new UtilEvalError(
					"LHS ("+field.getName()+") can't access field: "+e2);
			}     
			catch( IllegalArgumentException e3) 
			{
				String type = val instanceof Primitive ?
					((Primitive)val).getType().getName()
					: val.getClass().getName();
				throw new UtilEvalError(
					"Argument type mismatch. " + (val == null ? "null" : type )
					+ " not assignable to field "+field.getName());
			}
		}
		else 
		if ( type == PROPERTY )
		{
			
			CollectionManager cm = CollectionManager.getCollectionManager();
			if ( cm.isMap( object ) )
				cm.putInMap( object, propName, val );
			else
				try {
					Reflect.setObjectProperty(object, propName, val);
				}
				catch(ReflectError e) {
					Interpreter.debug("Assignment: " + e.getMessage());
					throw new UtilEvalError("No such property: " + propName);
				}
		} else 
		if ( type == INDEX )
			try {
				Reflect.setIndex(object, index, val);
			} catch ( UtilTargetError e1 ) { 
				throw e1;
			} catch ( Exception e ) {
				throw new UtilEvalError("Assignment: " + e.getMessage());
			}
		else
			throw new InterpreterError("unknown lhs");

		return val;
	}

	public String toString() { 
		return "LHS: "
			+((field!=null)? "field = "+field.toString():"")
			+(varName!=null ? " varName = "+varName: "")
			+(nameSpace!=null ? " nameSpace = "+nameSpace.toString(): "");
	}
}

