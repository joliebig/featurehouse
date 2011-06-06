


package bsh;

import java.util.Vector;


class BSHMethodDeclaration extends SimpleNode
{
	String name;
	BSHFormalParameters params;
	BSHBlock block;
	Object returnType; 	

	BSHMethodDeclaration(int id)
	{
		super(id);
	}

	
	public Object eval( NameSpace namespace )  
		throws EvalError
	{
		if ( block == null ) 
		{
			
			

			if(jjtGetNumChildren() == 3)
			{
				returnType = 
					((BSHReturnType)jjtGetChild(0)).getReturnType( namespace );
				params = (BSHFormalParameters)jjtGetChild(1);
				block = (BSHBlock)jjtGetChild(2);
			}
			else
			{
				params = (BSHFormalParameters)jjtGetChild(0);
				block = (BSHBlock)jjtGetChild(1);
			}
			params.eval( namespace );

			
			if ( Interpreter.strictJava )
			{
				for(int i=0; i<params.argTypes.length; i++)
					if ( params.argTypes[i] == null )
						throw new EvalError(
					"(Strict Java Mode) Undeclared argument type, parameter: " +
						params.argNames[i] + " in method: " 
						+ name, this );

				if ( returnType == null )
					throw new EvalError(
					"(Strict Java Mode) Undeclared return type for method: "
						+ name, this );
			}
		}

		
		





		namespace.setMethod( name, new BshMethod( this, namespace ) );

		return Primitive.VOID;
	}

	public String toString() {
		return "MethodDeclaration: "+name;
	}
}
