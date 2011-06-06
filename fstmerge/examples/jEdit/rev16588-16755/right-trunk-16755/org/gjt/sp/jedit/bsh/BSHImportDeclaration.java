


package org.gjt.sp.jedit.bsh;

class BSHImportDeclaration extends SimpleNode
{
	public boolean importPackage;
	public boolean staticImport;
	public boolean superImport;

	BSHImportDeclaration(int id) { super(id); }

	public Object eval( CallStack callstack, Interpreter interpreter) 
		throws EvalError
	{
		NameSpace namespace = callstack.top();
		if ( superImport )
			try {
				namespace.doSuperImport();
			} catch ( UtilEvalError e ) {
				throw e.toEvalError( this, callstack  );
			}
		else 
		{
			if ( staticImport )
			{
				if ( importPackage )
				{
					Class clas = ((BSHAmbiguousName)jjtGetChild(0)).toClass( 
						callstack, interpreter );
					namespace.importStatic( clas );
				} else
					throw new EvalError( 
						"static field imports not supported yet", 
						this, callstack );
			} else 
			{
				String name = ((BSHAmbiguousName)jjtGetChild(0)).text;
				if ( importPackage )
					namespace.importPackage(name);
				else
					namespace.importClass(name);
			}
		}

        return Primitive.VOID;
	}
}

