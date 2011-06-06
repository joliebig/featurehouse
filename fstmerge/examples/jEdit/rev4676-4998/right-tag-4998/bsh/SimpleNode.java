


package bsh;

class SimpleNode implements Node {
	protected Node parent;
	protected Node[] children;
	protected int id;
	Token firstToken, lastToken;

	
	String sourceFile;

	public SimpleNode(int i) {
		id = i;
	}

	public void jjtOpen() { }
	public void jjtClose() { }

	public void jjtSetParent(Node n) { parent = n; }
	public Node jjtGetParent() { return parent; }
	

	public void jjtAddChild(Node n, int i)
	{
		if (children == null)
			children = new Node[i + 1];
		else
			if (i >= children.length)
			{
				Node c[] = new Node[i + 1];
				System.arraycopy(children, 0, c, 0, children.length);
				children = c;
			}

		children[i] = n;
	}

	public Node jjtGetChild(int i) { 
		return children[i]; 
	}
	public SimpleNode getChild( int i ) {
		return (SimpleNode)jjtGetChild(i);
	}

	public int jjtGetNumChildren() {
		return (children == null) ? 0 : children.length;
	}

	
	public String toString() { return ParserTreeConstants.jjtNodeName[id]; }
	public String toString(String prefix) { return prefix + toString(); }

	
	public void dump(String prefix)
	{
		System.out.println(toString(prefix));
		if(children != null)
		{
			for(int i = 0; i < children.length; ++i)
			{
				SimpleNode n = (SimpleNode)children[i];
				if (n != null)
				{
					n.dump(prefix + " ");
				}
			}
		}
	}

	

	
	public void prune() {
		jjtSetParent( null );
	}

	
	public Object eval( NameSpace namespace ) 
		throws EvalError
	{
		throw new EvalError(
			"Unimplemented or inappropriate for " + getClass().getName());
	}

	
	public Object eval( CallStack callstack, Interpreter interpreter ) 
		throws EvalError
	{
		
		return eval( callstack.top() );
	}

	
	public void setSourceFile( String sourceFile ) {
		this.sourceFile = sourceFile;
	}

	
	public String getSourceFile() {
		if ( sourceFile == null )
			if ( parent != null )
				return ((SimpleNode)parent).getSourceFile();
			else
				return "<unknown file>";
		else
			return sourceFile;
	}

	
	public int getLineNumber() {
		return firstToken.beginLine;
	}

	
	public String getText() 
	{
		StringBuffer text = new StringBuffer();
		Token t = firstToken;
		while ( t!=null ) {
			text.append(t.image);
			if ( !t.image.equals(".") )
				text.append(" ");
			if ( t==lastToken ||
				t.image.equals("{") || t.image.equals(";") )
				break;
			t=t.next;
		}
			
		return text.toString();
	}
}

