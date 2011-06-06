package bsh;

public class Variable implements java.io.Serializable 
{
	static final int DECLARATION=0, ASSIGNMENT=1;
	
	String name;
	Class type = null;
	String typeDescriptor;
	Object value;
	Modifiers modifiers;
	LHS lhs;

	Variable( String name, Class type, LHS lhs ) 
	{
		this.name = name;
		this.lhs = lhs;
		this.type = type;
	}
	
	Variable( String name, Object value, Modifiers modifiers )
		throws UtilEvalError
	{
		this( name, (Class)null, value, modifiers );
	}

	
	Variable( 
		String name, String typeDescriptor, Object value, Modifiers modifiers 
	)
		throws UtilEvalError
	{
		this( name, (Class)null, value, modifiers );
		this.typeDescriptor = typeDescriptor;
	}

	
	Variable( String name, Class type, Object value, Modifiers modifiers )
		throws UtilEvalError
	{

		this.name=name;
		this.type =	type;
		this.modifiers = modifiers;
		setValue( value, DECLARATION );
	}

	
	public void setValue( Object value, int context ) 
		throws UtilEvalError
	{

		
		if ( hasModifier("final") && this.value != null )
			throw new UtilEvalError ("Final variable, can't re-assign.");

		if ( value == null )
			value = Primitive.getDefaultValue( type );

		if ( lhs != null )
		{
			lhs.assign( value, false );
			return;
		}

		if ( type != null )
			value = Types.castObject( value, type, 
				context == DECLARATION ? Types.CAST : Types.ASSIGNMENT
			);

		this.value= value;
	}

	Object getValue() 
		throws UtilEvalError
	{ 
		if ( lhs != null )
			return lhs.getValue();

		return value; 
	}

	
	public Class getType() { return type;	}

	public String getTypeDescriptor() { return typeDescriptor; }

	public Modifiers getModifiers() { return modifiers; }

	public String getName() { return name; }

	public boolean hasModifier( String name ) {
		return modifiers != null && modifiers.hasModifier(name);
	}

	public String toString() { 
		return "Variable: "+super.toString()+" "+name+", type:"+type
			+", value:"+value +", lhs = "+lhs;
	}
}
