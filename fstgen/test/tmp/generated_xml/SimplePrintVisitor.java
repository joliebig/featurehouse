package tmp.generated_xml;

import java.io.PrintStream;

import de.ovgu.cide.fstgen.ast.AbstractFSTPrintVisitor;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class SimplePrintVisitor extends AbstractFSTPrintVisitor  {
	public SimplePrintVisitor(PrintStream out) {
		super(out); generateSpaces=false;
	}
	public SimplePrintVisitor() {
		super(); generateSpaces=false;
	}
	public boolean visit(FSTNonTerminal nonTerminal) {
		if (nonTerminal.getType().equals("Document")) {
			{
				FSTNode v=getChild(nonTerminal, "Prolog");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "Element");
				if (v!=null) {
					v.accept(this);
				}
			}
			for (FSTNode v : getChildren(nonTerminal,"Misc")) {
				v.accept(this);
			}
			return false;
		}
		if (nonTerminal.getType().equals("ElementContainer1")) {
			{
				FSTNode v=getChild(nonTerminal, "EmptyElemTag");
				if (v!=null) {
					v.accept(this);
				}
			}
			return false;
		}
		if (nonTerminal.getType().equals("ElementContainer2")) {
			{
				FSTNode v=getChild(nonTerminal, "STag");
				if (v!=null) {
					v.accept(this);
				}
			}
			for (FSTNode v : getChildren(nonTerminal,"Content")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "ETag");
				if (v!=null) {
					v.accept(this);
				}
			}
			return false;
		}
		if (nonTerminal.getType().equals("EmptyElemTag")) {
			printToken("<");
			{
				FSTNode v=getChild(nonTerminal, "ElementId");
				if (v!=null) {
					v.accept(this);
				}
			}
			for (FSTNode v : getChildren(nonTerminal,"Attribute")) {
				v.accept(this);
			}
			printToken("/>");
			return false;
		}
		if (nonTerminal.getType().equals("STag")) {
			printToken("<");
			{
				FSTNode v=getChild(nonTerminal, "ElementId");
				if (v!=null) {
					v.accept(this);
				}
			}
			for (FSTNode v : getChildren(nonTerminal,"Attribute")) {
				v.accept(this);
			}
			printToken(">");
			return false;
		}
		if (nonTerminal.getType().equals("ElementContent")) {
			{
				FSTNode v=getChild(nonTerminal, "Element");
				if (v!=null) {
					v.accept(this);
				}
			}
			return false;
		}
		throw new RuntimeException("Unknown Non Terminal in FST "+nonTerminal);
	}
	protected boolean isSubtype(String type, String expectedType) {
		if (type.equals(expectedType)) return true;
		if (type.equals("ElementContent") && expectedType.equals("Content")) return true;
		if (type.equals("CommentContent") && expectedType.equals("Content")) return true;
		if (type.equals("Misc3") && expectedType.equals("Misc")) return true;
		if (type.equals("PCDataContent") && expectedType.equals("Content")) return true;
		if (type.equals("Misc2") && expectedType.equals("Misc")) return true;
		if (type.equals("ElementContainer2") && expectedType.equals("Element")) return true;
		if (type.equals("ElementContainer1") && expectedType.equals("Element")) return true;
		if (type.equals("Misc1") && expectedType.equals("Misc")) return true;
		if (type.equals("CDSectContent") && expectedType.equals("Content")) return true;
		return false;
	}
}
