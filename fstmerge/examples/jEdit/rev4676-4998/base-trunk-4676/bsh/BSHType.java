


package bsh;

import java.lang.reflect.Array;

class BSHType extends SimpleNode 
	implements BshClassManager.Listener
{
	
	private Class baseType;
	
    private int arrayDims;

	
    private Class type;

    BSHType(int id) { 
		super(id); 
		BshClassManager.addCMListener(this);
	}

	
    public void addArrayDimension() { 
		arrayDims++; 
	}

    
    public Class getType( NameSpace namespace ) 
		throws EvalError
    {
        
		if (type != null)
			return type;

        
        SimpleNode node = (SimpleNode)jjtGetChild(0);

        if(node instanceof BSHPrimitiveType)
            baseType = ((BSHPrimitiveType)node).getType();
        else 
            baseType = ((BSHAmbiguousName)node).toClass( namespace );

        if(arrayDims > 0) {
            try {
                
				
                int[] dims = new int[arrayDims]; 
                Object obj = Array.newInstance(baseType, dims);
                type = obj.getClass(); 
            } catch(Exception e) {
                throw new EvalError("Couldn't construct array type", this);
            }
        } else
            type = baseType;

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
}
