


package bsh;

import java.lang.reflect.Array;


class BSHArrayDimensions extends SimpleNode
{
	public Class baseType;
    public int numDefinedDims;
    public int numUndefinedDims;
	
	public int [] definedDimensions;  

    BSHArrayDimensions(int id) { super(id); }

    public void addDefinedDimension() { numDefinedDims++; }
    public void addUndefinedDimension() { numUndefinedDims++; }

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
					"Internal Array Eval err:  unknown base type", 
					this, callstack );

			Object initValue = ((BSHArrayInitializer)child).eval(
				baseType, numUndefinedDims, callstack, interpreter);

			Class arrayClass = initValue.getClass();
			int actualDimensions = Reflect.getArrayDimensions(arrayClass);
			definedDimensions = new int[ actualDimensions ];

			
			
			if ( definedDimensions.length != numUndefinedDims )
				throw new EvalError(
				"Incompatible initializer. Allocation calls for a " + 
				numUndefinedDims+ " dimensional array, but initializer is a " +
					actualDimensions + " dimensional array", this, callstack );

			
			Object arraySlice = initValue;
			for ( int i = 0; i < definedDimensions.length; i++ ) {
				definedDimensions[i] = Array.getLength( arraySlice );
				if ( definedDimensions[i] > 0 )
					arraySlice = Array.get(arraySlice, 0);
			}

			return initValue;
		}
		else 
		
		{
			definedDimensions = new int[ numDefinedDims ];

			for(int i = 0; i < numDefinedDims; i++)
			{
				try {
					Object length = ((SimpleNode)jjtGetChild(i)).eval(
						callstack, interpreter);
					definedDimensions[i] = ((Primitive)length).intValue();
				}
				catch(Exception e)
				{
					throw new EvalError(
						"Array index: " + i + 
						" does not evaluate to an integer", this, callstack );
				}
			}
		}

        return Primitive.VOID;
    }
}
