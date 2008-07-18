package tmp.generated_phaskell;

import java.util.*;
import cide.gast.*;

import java.io.PrintStream;

import cide.languages.*;

import de.ovgu.cide.fstgen.ast.*;

public class SimplePrintVisitor extends AbstractFSTPrintVisitor  {
	public SimplePrintVisitor(PrintStream out) {
		super(out); generateSpaces=true;
	}
	public SimplePrintVisitor() {
		super(); generateSpaces=true;
	}
	public boolean visit(FSTNonTerminal nonTerminal) {
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
