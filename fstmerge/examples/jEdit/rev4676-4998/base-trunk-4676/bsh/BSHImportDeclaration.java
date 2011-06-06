


package bsh;

class BSHImportDeclaration extends SimpleNode
{
	public boolean importPackage;
	public boolean superImport;

	BSHImportDeclaration(int id) { super(id); }

	public Object eval( CallStack callstack, Interpreter interpreter) 
		throws EvalError
	{
		if ( superImport )
			NameSpace.doSuperImport();
		else {
			NameSpace namespace = callstack.top();
			String name = 
				((BSHAmbiguousName)jjtGetChild(0)).getName(namespace).value;

			if ( importPackage )
				namespace.importPackage(name);
			else
				namespace.importClass(name);
		}

        return Primitive.VOID;
	}
}

