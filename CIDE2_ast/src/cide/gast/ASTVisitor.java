package cide.gast;

public class ASTVisitor implements IASTVisitor {

	/* (non-Javadoc)
	 * @see cide.gast.IASTVisitor#visit(cide.gast.ASTNode)
	 */
	public boolean visit(ASTNode node){
		return true;
	}
	
	/* (non-Javadoc)
	 * @see cide.gast.IASTVisitor#postVisit(cide.gast.ASTNode)
	 */
	public void postVisit(ASTNode node){
		
	}
	
}
