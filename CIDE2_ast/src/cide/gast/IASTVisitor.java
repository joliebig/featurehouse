package cide.gast;

public interface IASTVisitor {

	public abstract boolean visit(ASTNode node);

	public abstract void postVisit(ASTNode node);

}