

package org.gjt.sp.jedit.bsh;



class BlockNameSpace extends NameSpace 
{
    public BlockNameSpace( NameSpace parent ) 
		throws EvalError
	{
		super( parent, parent.getName()+ "/BlockNameSpace" );
    }

	
	
    public void	setVariable( 
		String name, Object value, boolean strictJava, boolean recurse ) 
		throws UtilEvalError 
	{
		if ( weHaveVar( name ) ) 
			
			super.setVariable( name, value, strictJava, false );
		else
			
			getParent().setVariable( name, value, strictJava, recurse );
    }

	
    public void	setBlockVariable( String name, Object value ) 
		throws UtilEvalError 
	{
		super.setVariable( name, value, false, false );
	}

	
	private boolean weHaveVar( String name ) 
	{
		
		try {
			return super.getVariableImpl( name, false ) != null;
		} catch ( UtilEvalError e ) { return false; }
	}



	
	
	

	
	
	private NameSpace getNonBlockParent() 
	{
		NameSpace parent = super.getParent();
		if ( parent instanceof BlockNameSpace )
			return ((BlockNameSpace)parent).getNonBlockParent();
		else
			return parent;
	}

	
    This getThis( Interpreter declaringInterpreter ) {
		return getNonBlockParent().getThis( declaringInterpreter );
	}

	
    public This getSuper( Interpreter declaringInterpreter ) {
		return getNonBlockParent().getSuper( declaringInterpreter );
	}

	
    public void	importClass(String name) {
		getParent().importClass( name );
	}

	
    public void	importPackage(String name) {
		getParent().importPackage( name );
	}

    public void	setMethod(String name, BshMethod method) 
		throws UtilEvalError
	{
		getParent().setMethod( name, method );
	}
}

