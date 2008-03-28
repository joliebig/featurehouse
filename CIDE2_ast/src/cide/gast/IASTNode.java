package cide.gast;

public interface IASTNode {

	public void accept(IASTVisitor visitor);

	public Property getProperty(String name);

	public ISourceFile getRoot();

	public ASTNode getParent();

	public String getId();

	public int getStartPosition();

	public int getLength();
	
	public ASTNode deepCopy();
}
