package tmp.generated_xmi;

import java.util.*;
import cide.gast.*;

import java.io.PrintStream;

import cide.languages.*;

import de.ovgu.cide.fstgen.ast.*;

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
		if (nonTerminal.getType().equals("ElementContent1")) {
			{
				FSTNode v=getChild(nonTerminal, "EmptyElemTag");
				if (v!=null) {
					v.accept(this);
				}
			}
			return false;
		}
		if (nonTerminal.getType().equals("ElementContent2")) {
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
		if (nonTerminal.getType().equals("Element")) {
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
		if (type.equals("ElementContent1") && expectedType.equals("Element")) return true;
		if (type.equals("ElementContent2") && expectedType.equals("Element")) return true;
		if (type.equals("Element") && expectedType.equals("Content")) return true;
		if (type.equals("CommentContent") && expectedType.equals("Content")) return true;
		if (type.equals("Misc3") && expectedType.equals("Misc")) return true;
		if (type.equals("PCDataContent") && expectedType.equals("Content")) return true;
		if (type.equals("Misc2") && expectedType.equals("Misc")) return true;
		if (type.equals("Attr_{<ATTR_NAME>}") && expectedType.equals("Attribute")) return true;
		if (type.equals("Misc1") && expectedType.equals("Misc")) return true;
		if (type.equals("CDSectContent") && expectedType.equals("Content")) return true;
		return false;
	}
}
