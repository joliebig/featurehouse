


package bsh;


class BSHCastExpression extends SimpleNode {

    public BSHCastExpression(int id) { super(id); }

	
	public Object eval(
		CallStack callstack, Interpreter interpreter ) throws EvalError
    {
		NameSpace namespace = callstack.top();
        Class toType = ((BSHType)jjtGetChild(0)).getType( 
			callstack, interpreter );
		SimpleNode expression = (SimpleNode)jjtGetChild(1);

        
        Object fromValue = expression.eval(callstack, interpreter);
        Class fromType = fromValue.getClass();

		try {
			return Types.castObject( fromValue, toType, Types.CAST );
		} catch ( UtilEvalError e ) {
			throw e.toEvalError( this, callstack  );
		}
    }

}
