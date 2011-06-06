


package org.gjt.sp.jedit.bsh;

import java.lang.reflect.Array;

class BSHType extends SimpleNode 
	implements BshClassManager.Listener
{
	
	private Class baseType;
	
    private int arrayDims;

	
    private Class type;

	String descriptor;

    BSHType(int id) { 
		super(id); 
	}

	
    public void addArrayDimension() { 
		arrayDims++; 
	}

	SimpleNode getTypeNode() {
        return (SimpleNode)jjtGetChild(0);
	}

    
    public String getTypeDescriptor( 
		CallStack callstack, Interpreter interpreter, String defaultPackage ) 
    {
        
		if ( descriptor != null )
			return descriptor;

		String descriptor;
        
        SimpleNode node = getTypeNode();
        if ( node instanceof BSHPrimitiveType )
            descriptor = getTypeDescriptor( ((BSHPrimitiveType)node).type );
        else 
		{
            String clasName = ((BSHAmbiguousName)node).text;
			BshClassManager bcm = interpreter.getClassManager();
			
			
			
			
			String definingClass = bcm.getClassBeingDefined( clasName );

            Class clas = null;
			if ( definingClass == null )
			{
				try {
					clas = ((BSHAmbiguousName)node).toClass( 
						callstack, interpreter );
				} catch ( EvalError e ) {
					
					
					
				}
			} else
				clasName = definingClass;

			if ( clas != null )
			{
				
            	descriptor = getTypeDescriptor( clas );
			}else
			{
				if ( defaultPackage == null || Name.isCompound( clasName ) )
            		descriptor = "L" + clasName.replace('.','/') + ";";
				else
            		descriptor = 
						"L"+defaultPackage.replace('.','/')+"/"+clasName + ";";
			}
		}

		for(int i=0; i<arrayDims; i++)
			descriptor = "["+descriptor;

		this.descriptor = descriptor;
	
        return descriptor;
    }

    public Class getType( CallStack callstack, Interpreter interpreter ) 
		throws EvalError
    {
        
		if ( type != null )
			return type;

        
        SimpleNode node = getTypeNode();
        if ( node instanceof BSHPrimitiveType )
            baseType = ((BSHPrimitiveType)node).getType();
        else 
            baseType = ((BSHAmbiguousName)node).toClass( 
				callstack, interpreter );

        if ( arrayDims > 0 ) {
            try {
                
				
                int[] dims = new int[arrayDims]; 
                Object obj = Array.newInstance(baseType, dims);
                type = obj.getClass(); 
            } catch(Exception e) {
                throw new EvalError("Couldn't construct array type", 
					this, callstack );
            }
        } else
            type = baseType;

		
		
		interpreter.getClassManager().addListener(this);

        return type;
    }

	
	public Class getBaseType() {
		return baseType;
	}
	
	public int getArrayDims() {
		return arrayDims;
	}

	public void classLoaderChanged() {
		type = null;
		baseType = null;
	}

	public static String getTypeDescriptor( Class clas ) 
	{
		if ( clas == Boolean.TYPE ) return "Z";
		if ( clas == Character.TYPE ) return "C"; 
		if ( clas == Byte.TYPE ) return "B";
		if ( clas == Short.TYPE ) return "S";
		if ( clas == Integer.TYPE ) return "I";
		if ( clas == Long.TYPE ) return "J";
		if ( clas == Float.TYPE ) return "F";
		if ( clas == Double.TYPE ) return "D";
		if ( clas == Void.TYPE ) return "V";
	
		String name = clas.getName().replace('.','/');

		if ( name.startsWith("[") || name.endsWith(";") )
			return name;
		else
			return "L"+ name.replace('.','/') +";";
	}
}
