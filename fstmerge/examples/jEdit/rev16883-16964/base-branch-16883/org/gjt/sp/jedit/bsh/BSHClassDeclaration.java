

package org.gjt.sp.jedit.bsh;


class BSHClassDeclaration extends SimpleNode
{
	
	static final String CLASSINITNAME = "_bshClassInit";

	String name;
	Modifiers modifiers;
	int numInterfaces;
	boolean extend;
	boolean isInterface;

	BSHClassDeclaration(int id) { super(id); }

	
	public Object eval( CallStack callstack, Interpreter interpreter )
		throws EvalError
	{
		int child = 0;

		
		Class superClass = null;
		if ( extend ) 
		{
			BSHAmbiguousName superNode = (BSHAmbiguousName)jjtGetChild(child++);
			superClass = superNode.toClass( callstack, interpreter );
		}

		
		Class [] interfaces = new Class[numInterfaces];
		for( int i=0; i<numInterfaces; i++) {
			BSHAmbiguousName node = (BSHAmbiguousName)jjtGetChild(child++);
			interfaces[i] = node.toClass(callstack, interpreter);
			if ( !interfaces[i].isInterface() )
				throw new EvalError(
					"Type: "+node.text+" is not an interface!", 
					this, callstack );
		}

		BSHBlock block;
		
		if ( child < jjtGetNumChildren() )
			block = (BSHBlock)jjtGetChild(child);
		else
			block = new BSHBlock( ParserTreeConstants.JJTBLOCK );

		try {
			return ClassGenerator.getClassGenerator().generateClass( 
				name, modifiers, interfaces, superClass, block, isInterface,
				callstack, interpreter );
		} catch ( UtilEvalError e ) {
			throw e.toEvalError( this, callstack );
		}

	}

	public String toString() {
		return "ClassDeclaration: "+name;
	}
}
