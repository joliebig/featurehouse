


package bsh;


class BSHVariableDeclarator extends SimpleNode
{
	
    public String name;

    BSHVariableDeclarator(int id) { super(id); }

	
    public Object eval( 
		BSHType typeNode, CallStack callstack, Interpreter interpreter)  
		throws EvalError
    {
		Object value = Primitive.VOID;

        if ( jjtGetNumChildren() > 0 ) 
		{
            SimpleNode initializer = (SimpleNode)jjtGetChild(0);

			
			if ( (typeNode != null) 
				&& initializer instanceof BSHArrayInitializer 
			)
            	value = ((BSHArrayInitializer)initializer).eval( 
					typeNode.getBaseType(), typeNode.getArrayDims(), 
					callstack, interpreter);
			else
				value = initializer.eval( callstack, interpreter);
		}

        return value;
    }
}
