


package bsh;

import java.lang.reflect.Array;


class BSHArrayDimensions extends SimpleNode
{
	public Class baseType;
    private int arrayDims;

	
	
	
	public int [] dimensions;  

    BSHArrayDimensions(int id) { super(id); }

    public void addArrayDimension() { arrayDims++; }

    public Object eval( 
			Class type, CallStack callstack, Interpreter interpreter ) 
		throws EvalError 
	{
		if ( Interpreter.DEBUG ) Interpreter.debug("array base type = "+type);
		baseType = type;
		return eval( callstack, interpreter );
	}

	
    public Object eval( CallStack callstack, Interpreter interpreter )  
		throws EvalError
    {
		SimpleNode child = (SimpleNode)jjtGetChild(0);

		if (child instanceof BSHArrayInitializer)
		
		{
			if ( baseType == null )
				throw new EvalError( 
					"Internal Array Eval err:  unknown base type", this);

			Object initValue = ((BSHArrayInitializer)child).eval(
				baseType, arrayDims, callstack, interpreter);

			Class arrayClass = initValue.getClass();
			dimensions = new int[
				Reflect.getArrayDimensions(arrayClass) ];

			
			if (dimensions.length != arrayDims)
				throw new EvalError(
				"Incompatible initializer. Allocation calls for a " + 
				arrayDims + " dimensional array, but initializer is a " +
					dimensions.length + " dimensional array", this);

			
			Object arraySlice = initValue;
			for(int i = 0; i < dimensions.length; i++) {
				dimensions[i] = Array.getLength( arraySlice );
				if ( dimensions[i] > 0 )
					arraySlice = Array.get(arraySlice, 0);
			}

			return initValue;
		}
		else 
		
		{
			dimensions = new int[ jjtGetNumChildren() ];
			for(int i = 0; i < dimensions.length; i++)
			{
				try {
					Object length = ((SimpleNode)jjtGetChild(i)).eval(
						callstack, interpreter);
					dimensions[i] = ((Primitive)length).intValue();
				}
				catch(Exception e)
				{
					throw new EvalError(
						"Array index: " + i + 
						" does not evaluate to an integer", this);
				}
			}
		}

        return Primitive.VOID;
    }
}
