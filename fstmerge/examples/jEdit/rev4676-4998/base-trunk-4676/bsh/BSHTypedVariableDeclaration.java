


package bsh;

class BSHTypedVariableDeclaration extends SimpleNode
{
    public boolean isFinal;
	
    BSHTypedVariableDeclaration(int id) { super(id); }

	
    public Object eval( CallStack callstack, Interpreter interpreter)  
		throws EvalError
    {
		try {
			NameSpace namespace = callstack.top();
			BSHType typeNode = ((BSHType)jjtGetChild(0));
			Class type = typeNode.getType( namespace );

			int n = jjtGetNumChildren();
			for (int i = 1; i < n; i++)
			{
				BSHVariableDeclarator dec = 
					(BSHVariableDeclarator)jjtGetChild(i);

				
				
				Object value = dec.eval( typeNode, callstack, interpreter);

				
				
				if ( value == Primitive.VOID ) 
					value = null;
				else 
				
				if ( value == Primitive.NULL ) {
					
				}
				else
				
				if ( canCastToDeclaredType( value, type ) )
					value = BSHCastExpression.castObject( value, type );
				else {
					
				}

				namespace.setTypedVariable( dec.name, type, value, isFinal );
			}
		} catch ( EvalError e ) {
			e.reThrow( "Typed variable declaration", this );
		}

        return Primitive.VOID;
    }

	
	
	boolean canCastToDeclaredType( Object value, Class toType ) {
		if ( !(value instanceof Primitive) )
			return false;
		Class fromType = ((Primitive)value).getType();
		
		if ( (toType==Byte.TYPE || toType==Short.TYPE || toType==Character.TYPE)
			&& fromType == Integer.TYPE 
		)
			return true;
		else
			return false;
	}

}
