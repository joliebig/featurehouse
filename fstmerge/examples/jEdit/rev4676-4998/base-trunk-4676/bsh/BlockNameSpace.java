


package	bsh;


class BlockNameSpace extends NameSpace 
{
	
	boolean initMode;

    public BlockNameSpace( NameSpace parent ) 
		throws EvalError
	{
		super( parent, parent.name + "/BlockNameSpace" );
    }

	
    public void	setVariable(String name, Object	o) throws EvalError {
		if ( weHaveVar( name ) || initMode ) 
			super.setVariable( name, o );
		else
			getParent().setVariable( name, o );
    }

	
	public void setInitMode( boolean b ) {
		initMode = b;
	}

	boolean weHaveVar( String name ) {
		return super.getVariableImpl( name, false ) != null;
	}

	
    public NameSpace getSuper() {
		return getParent().getSuper();
	}

	
    This getThis( Interpreter declaringInterpreter ) {
		return getParent().getThis( declaringInterpreter );
	}

	
    public void	importClass(String name) {
		getParent().importClass( name );
	}

	
    public void	importPackage(String name) {
		getParent().importPackage( name );
	}

}

