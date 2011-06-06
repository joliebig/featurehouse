


package bsh;

import java.lang.reflect.Field;
import java.util.Hashtable;


class LHS implements ParserConstants, java.io.Serializable
{
	NameSpace nameSpace;

	static final int
		VARIABLE = 0,
		FIELD = 1,
		PROPERTY = 2,
		INDEX = 3;

	int type;

	String varName;
	String propName;
	Field field;
	Object object;
	int index;

	LHS(NameSpace nameSpace, String varName)
	{
		type = VARIABLE;
		this.varName = varName;
		this.nameSpace = nameSpace;
	}

	
	LHS(Field field)
	{
		type = FIELD;
		this.object = null;
		this.field = field;
	}

	
	LHS(Object object, Field field)
	{
		if(object == null)
			throw new NullPointerException("constructed empty LHS");

		type = FIELD;
		this.object = object;
		this.field = field;
	}

	
	LHS(Object object, String propName)
	{
		if(object == null)
			throw new NullPointerException("constructed empty LHS");

		type = PROPERTY;
		this.object = object;
		this.propName = propName;
	}

	
	LHS(Object array, int index)
	{
		if(array == null)
			throw new NullPointerException("constructed empty LHS");

		type = INDEX;
		this.object = array;
		this.index = index;
	}

	public Object getValue() throws EvalError
	{
		if(type == VARIABLE)
			return nameSpace.getVariable(varName);
		else if (type == FIELD)
			try {
				return field.get(object);
			}
			catch(IllegalAccessException e2) {
				throw new EvalError("Can't read field: " + field);
			}
		else if(type == PROPERTY)
			try {
				return Reflect.getObjectProperty(object, propName);
			}
			catch(ReflectError e) {
				Interpreter.debug(e.getMessage());
				throw new EvalError("No such property: " + propName);
			}
		else if(type == INDEX)
			try
			{
				return Reflect.getIndex(object, index);
			}
			catch(Exception e)
			{
				throw new EvalError("Array access: " + e);
			}

		throw new InterpreterError("LHS type");
	}

	public Object assign( Object val ) throws EvalError
	{
		if ( type == VARIABLE )
			nameSpace.setVariable(varName, val);
		else 
		if ( type == FIELD )
			try {
				if(val instanceof Primitive)
					val = ((Primitive)val).getValue();

				field.set(object, val);
				return val;
			}
			catch( NullPointerException e) {   
    			throw new EvalError(
					"LHS ("+field.getName()+") not a static field.");
			}     
   			catch( IllegalAccessException e2) {   
				throw new EvalError(
					"LHS ("+field.getName()+") can't access field.");
			}     
			catch( IllegalArgumentException e3) {
				throw new EvalError(
					"Argument type mismatch. "
					+ (val == null ? "null" : val.getClass().getName())
					+ " not assignable to field "+field.getName());
			}

		else if(type == PROPERTY)
			if ( object instanceof Hashtable )
				((Hashtable)object).put(propName, val);
			else
				try {
					Reflect.setObjectProperty(object, propName, val);
				}
				catch(ReflectError e) {
					Interpreter.debug("Assignment: " + e.getMessage());
					throw new EvalError("No such property: " + propName);
				}
		else if(type == INDEX)
			try {
				Reflect.setIndex(object, index, val);
			} catch(TargetError e1) { 
				throw e1;
			} catch(Exception e) {
				throw new EvalError("Assignment: " + e.getMessage());
			}

		return val;
	}

	public String toString() { return "LHS"; }
}

