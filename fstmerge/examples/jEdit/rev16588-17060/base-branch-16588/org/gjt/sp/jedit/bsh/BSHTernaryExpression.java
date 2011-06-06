


package org.gjt.sp.jedit.bsh;


class BSHTernaryExpression extends SimpleNode {

    BSHTernaryExpression(int id) { super(id); }

    public Object eval( CallStack callstack, Interpreter interpreter) 
		throws EvalError
    {
        SimpleNode
			cond = (SimpleNode)jjtGetChild(0),
			evalTrue = (SimpleNode)jjtGetChild(1),
			evalFalse = (SimpleNode)jjtGetChild(2);

		if ( BSHIfStatement.evaluateCondition( cond, callstack, interpreter ) )
			return evalTrue.eval( callstack, interpreter );
		else
			return evalFalse.eval( callstack, interpreter );
    }

}
