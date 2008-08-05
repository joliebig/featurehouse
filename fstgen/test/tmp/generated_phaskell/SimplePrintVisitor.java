package tmp.generated_phaskell;

import java.io.PrintStream;
import java.util.Iterator;

import de.ovgu.cide.fstgen.ast.AbstractFSTPrintVisitor;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class SimplePrintVisitor extends AbstractFSTPrintVisitor  {
	public SimplePrintVisitor(PrintStream out) {
		super(out); generateSpaces=true;
	}
	public SimplePrintVisitor() {
		super(); generateSpaces=true;
	}
	public boolean visit(FSTNonTerminal nonTerminal) {
		if (nonTerminal.getType().equals("module")) {
			printToken("module");
			{
				FSTNode v=getChild(nonTerminal, "modid");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "exports");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintNewLine();
			printToken("where");
			{
				FSTNode v=getChild(nonTerminal, "body");
				if (v!=null) {
					v.accept(this);
				}
			}
			return false;
		}
		if (nonTerminal.getType().equals("body1")) {
			printToken("{");
			hintIncIndent();
			hintNewLine();
			{
				FSTNode v=getChild(nonTerminal, "impdecls");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "topdecls");
				if (v!=null) {
					printToken(";");
					hintNewLine();
					hintNewLine();
					v.accept(this);
				}
			}
			hintDecIndent();
			hintNewLine();
			printToken("}");
			return false;
		}
		if (nonTerminal.getType().equals("body2")) {
			printToken("{");
			hintIncIndent();
			hintNewLine();
			{
				FSTNode v=getChild(nonTerminal, "topdecls");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintDecIndent();
			hintNewLine();
			printToken("}");
			return false;
		}
		if (nonTerminal.getType().equals("topdecls")) {
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "topdecl").iterator();
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				printToken(";");
				hintNewLine();
				hintNewLine();
				listElements.next().accept(this);
			}
			return false;
		}
		if (nonTerminal.getType().equals("typedecl")) {
			printToken("type");
			{
				FSTNode v=getChild(nonTerminal, "simpletype");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "declrhs");
				if (v!=null) {
					v.accept(this);
				}
			}
			return false;
		}
		if (nonTerminal.getType().equals("datadecl")) {
			printToken("data");
			{
				FSTNode v=getChild(nonTerminal, "optContext");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "simpletype");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("=");
			{
				FSTNode v=getChild(nonTerminal, "constrs");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "deriving");
				if (v!=null) {
					v.accept(this);
				}
			}
			return false;
		}
		if (nonTerminal.getType().equals("newtypedecl")) {
			printToken("newtype");
			{
				FSTNode v=getChild(nonTerminal, "optContext");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "simpletype");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "declrhs");
				if (v!=null) {
					v.accept(this);
				}
			}
			return false;
		}
		if (nonTerminal.getType().equals("classdecl")) {
			printToken("class");
			{
				FSTNode v=getChild(nonTerminal, "optContext");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "conid");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "tyvar");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "cdecls");
				if (v!=null) {
					printToken("where");
					v.accept(this);
				}
			}
			return false;
		}
		if (nonTerminal.getType().equals("instancedecl")) {
			printToken("instance");
			{
				FSTNode v=getChild(nonTerminal, "optContext");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "qconid");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "inst");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "block");
				if (v!=null) {
					printToken("where");
					v.accept(this);
				}
			}
			return false;
		}
		if (nonTerminal.getType().equals("constrs")) {
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "constr").iterator();
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				printToken("|");
				listElements.next().accept(this);
			}
			return false;
		}
		throw new RuntimeException("Unknown Non Terminal in FST "+nonTerminal);
	}
	protected boolean isSubtype(String type, String expectedType) {
		if (type.equals(expectedType)) return true;
		if (type.equals("imp1") && expectedType.equals("imp")) return true;
		if (type.equals("fixity2") && expectedType.equals("fixity")) return true;
		if (type.equals("op1") && expectedType.equals("op")) return true;
		if (type.equals("export2") && expectedType.equals("export")) return true;
		if (type.equals("body2") && expectedType.equals("body")) return true;
		if (type.equals("cdecl1") && expectedType.equals("cdecl")) return true;
		if (type.equals("nonStandardDeclaration") && expectedType.equals("decl")) return true;
		if (type.equals("funlhsL2") && expectedType.equals("funlhsL")) return true;
		if (type.equals("decl5") && expectedType.equals("decl")) return true;
		if (type.equals("qvar1") && expectedType.equals("qvar")) return true;
		if (type.equals("fixityDeclaration") && expectedType.equals("decl")) return true;
		if (type.equals("conop1") && expectedType.equals("conop")) return true;
		if (type.equals("datadecl") && expectedType.equals("topdecl")) return true;
		if (type.equals("impdecl1") && expectedType.equals("impdecl")) return true;
		if (type.equals("inst3") && expectedType.equals("inst")) return true;
		if (type.equals("op2") && expectedType.equals("op")) return true;
		if (type.equals("details1") && expectedType.equals("details")) return true;
		if (type.equals("classdecl") && expectedType.equals("topdecl")) return true;
		if (type.equals("fixity3") && expectedType.equals("fixity")) return true;
		if (type.equals("export3") && expectedType.equals("export")) return true;
		if (type.equals("body1") && expectedType.equals("body")) return true;
		if (type.equals("inst2") && expectedType.equals("inst")) return true;
		if (type.equals("conop2") && expectedType.equals("conop")) return true;
		if (type.equals("declaration") && expectedType.equals("topdecl")) return true;
		if (type.equals("qvar2") && expectedType.equals("qvar")) return true;
		if (type.equals("funlhsR1") && expectedType.equals("funlhsR")) return true;
		if (type.equals("varop2") && expectedType.equals("varop")) return true;
		if (type.equals("cname2") && expectedType.equals("cname")) return true;
		if (type.equals("varop1") && expectedType.equals("varop")) return true;
		if (type.equals("var2") && expectedType.equals("var")) return true;
		if (type.equals("newtypedecl") && expectedType.equals("topdecl")) return true;
		if (type.equals("conP2") && expectedType.equals("conP")) return true;
		if (type.equals("funlhsL1") && expectedType.equals("funlhsL")) return true;
		if (type.equals("typedecl") && expectedType.equals("topdecl")) return true;
		if (type.equals("funlhsR2") && expectedType.equals("funlhsR")) return true;
		if (type.equals("valueDeclaration") && expectedType.equals("decl")) return true;
		if (type.equals("inst1") && expectedType.equals("inst")) return true;
		if (type.equals("cname1") && expectedType.equals("cname")) return true;
		if (type.equals("var1") && expectedType.equals("var")) return true;
		if (type.equals("imp2") && expectedType.equals("imp")) return true;
		if (type.equals("fixity1") && expectedType.equals("fixity")) return true;
		if (type.equals("cdecl2") && expectedType.equals("cdecl")) return true;
		if (type.equals("conP1") && expectedType.equals("conP")) return true;
		if (type.equals("typeSignature") && expectedType.equals("decl")) return true;
		if (type.equals("instancedecl") && expectedType.equals("topdecl")) return true;
		if (type.equals("details2") && expectedType.equals("details")) return true;
		if (type.equals("export1") && expectedType.equals("export")) return true;
		if (type.equals("impdecl2") && expectedType.equals("impdecl")) return true;
		if (type.equals("defaultdecl") && expectedType.equals("topdecl")) return true;
		return false;
	}
}
