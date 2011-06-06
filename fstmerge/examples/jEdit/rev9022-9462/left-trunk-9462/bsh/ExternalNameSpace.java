package	bsh;

import java.util.*;



public class ExternalNameSpace extends NameSpace
{
	private Map externalMap;

    public ExternalNameSpace() 
	{
		this( null, "External Map Namespace", null );
	}

	
    public ExternalNameSpace( NameSpace parent, String name, Map externalMap ) 
	{
		super( parent, name );

		if ( externalMap == null )
			externalMap = new HashMap();
			
		this.externalMap = externalMap;

	}

	
	public Map getMap() { return externalMap; }

	
	public void setMap( Map map ) 
	{ 
		
		
		this.externalMap = null; 
		clear();
		this.externalMap = map ; 
	}

	
    void setVariable( 
		String name, Object value, boolean strictJava, boolean recurse ) 
		throws UtilEvalError 
	{
		super.setVariable( name, value, strictJava, recurse );
		putExternalMap( name, value );
	}

	
	public void unsetVariable( String name )
	{
		super.unsetVariable( name );
		externalMap.remove( name );
	}

	
	public String [] getVariableNames() 
	{
		
		Set nameSet = new HashSet();
		String [] nsNames = super.getVariableNames();
		nameSet.addAll( Arrays.asList( nsNames ) );
		nameSet.addAll( externalMap.keySet() );
		return (String [])nameSet.toArray( new String[0] );
	}

	
	
    protected Variable getVariableImpl( String name, boolean recurse ) 
		throws UtilEvalError
	{
		
		Object value = externalMap.get( name );

		Variable var;
		if ( value == null ) 
		{
			
			
			
			super.unsetVariable( name ); 

			
			var = super.getVariableImpl( name, recurse );
		} else
		{
			
			
			Variable localVar = super.getVariableImpl( name, false );

			
			
			
			if ( localVar == null ) 
				var = new Variable( name, (Class)null, value, (Modifiers)null );
			else
				var = localVar;
		}

		return var;
    }
	
	
	
	public Variable [] getDeclaredVariables() 
	{
		return super.getDeclaredVariables();
	}

    
    public void	setTypedVariable(
		String	name, Class type, Object value,	Modifiers modifiers )
		throws UtilEvalError 
	{
		super.setTypedVariable( name, type, value, modifiers );
		putExternalMap( name, value );
    }

	
    public void	setMethod( String name, BshMethod method )
		throws UtilEvalError
	{
		super.setMethod( name, method );
    }

	
    public BshMethod getMethod( 
		String name, Class [] sig, boolean declaredOnly ) 
		throws UtilEvalError
	{
		return super.getMethod( name, sig, declaredOnly );
    }


	
	protected void getAllNamesAux( Vector vec ) 
	{
		super.getAllNamesAux( vec );
	}

	
	public void clear() 
	{
		super.clear();
		externalMap.clear();
	}

	
	protected void putExternalMap( String name, Object value ) 
	{
		if ( value instanceof Variable )
			try {
				value = unwrapVariable( (Variable)value );
			} catch ( UtilEvalError ute ) {
				
				
				
				throw new InterpreterError("unexpected UtilEvalError");
			}

		if ( value instanceof Primitive )
			value = Primitive.unwrap( (Primitive)value );

		externalMap.put( name, value );
	}
}

