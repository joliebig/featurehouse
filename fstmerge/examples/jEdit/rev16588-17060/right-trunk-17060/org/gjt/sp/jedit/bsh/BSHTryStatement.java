


package org.gjt.sp.jedit.bsh;

import java.util.Vector;

class BSHTryStatement extends SimpleNode
{
	BSHTryStatement(int id)
	{
		super(id);
	}

	public Object eval( CallStack callstack, Interpreter interpreter)  
		throws EvalError
	{
		BSHBlock tryBlock = ((BSHBlock)jjtGetChild(0));

		Vector catchParams = new Vector();
		Vector catchBlocks = new Vector();

		int nchild = jjtGetNumChildren();
		Node node = null;
		int i=1;
		while((i < nchild) && ((node = jjtGetChild(i++)) instanceof BSHFormalParameter))
		{
			catchParams.addElement(node);
			catchBlocks.addElement(jjtGetChild(i++));
			node = null;
		}
		
		BSHBlock finallyBlock = null;
		if(node != null)
			finallyBlock = (BSHBlock)node;



		TargetError target = null;
		Throwable thrown = null;
		Object ret = null;

		
		int callstackDepth = callstack.depth();
		try {
			ret = tryBlock.eval(callstack, interpreter);
		}
		catch( TargetError e ) {
			target = e;
			String stackInfo = "Bsh Stack: ";
			while ( callstack.depth() > callstackDepth )
				stackInfo += "\t" + callstack.pop() +"\n";
		}

		
		if ( target != null )
			thrown = target.getTarget();

		
		
		if (thrown != null) 
		{
			int n = catchParams.size();
			for(i=0; i<n; i++)
			{
				
				BSHFormalParameter fp = 
					(BSHFormalParameter)catchParams.elementAt(i);

				
				
				
				
				fp.eval( callstack, interpreter );

				if ( fp.type == null && interpreter.getStrictJava() )
					throw new EvalError(
						"(Strict Java) Untyped catch block", this, callstack );

				
				if ( fp.type != null ) 
					try {
						thrown = (Throwable)Types.castObject(
							thrown, fp.type, Types.ASSIGNMENT );
					} catch( UtilEvalError e ) {
						
						continue;
					}

				
				BSHBlock cb = (BSHBlock)(catchBlocks.elementAt(i));

				
				
				

				NameSpace enclosingNameSpace = callstack.top();
				BlockNameSpace cbNameSpace = 
					new BlockNameSpace( enclosingNameSpace );

				try {
					if ( fp.type == BSHFormalParameter.UNTYPED )
						
						cbNameSpace.setBlockVariable( fp.name, thrown );
					else
					{
						
						Modifiers modifiers = new Modifiers();
						cbNameSpace.setTypedVariable(
							fp.name, fp.type, thrown, new Modifiers() );
					}
				} catch ( UtilEvalError e ) {
					throw new InterpreterError(
						"Unable to set var in catch block namespace." );
				}

				
				callstack.swap( cbNameSpace );
				try {
					ret = cb.eval( callstack, interpreter );
				} finally {
					
					callstack.swap( enclosingNameSpace );
				}

				target = null;  
				break;
			}
		}

		
		if(finallyBlock != null)
			ret = finallyBlock.eval(callstack, interpreter);

		
		if(target != null)
			throw target;

		if(ret instanceof ReturnControl)
			return ret;
		else	
			return Primitive.VOID;
	}
}
