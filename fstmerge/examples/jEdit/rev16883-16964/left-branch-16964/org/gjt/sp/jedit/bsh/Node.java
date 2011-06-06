




package org.gjt.sp.jedit.bsh;


interface Node extends java.io.Serializable
{

	public void jjtOpen();


	public void jjtClose();


	public void jjtSetParent(Node n);
	public Node jjtGetParent();


	public void jjtAddChild(Node n, int i);


	public Node jjtGetChild(int i);


	public int jjtGetNumChildren();
}

