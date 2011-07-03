package tmp.generated_stratego;

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
		if (type.equals("Grammer1") && expectedType.equals("Grammer")) return true;
		if (type.equals("ProdPart4") && expectedType.equals("ProdPart")) return true;
		if (type.equals("SortOp1") && expectedType.equals("SortOp")) return true;
		if (type.equals("Attribute2") && expectedType.equals("Attribute")) return true;
		if (type.equals("Attribute6") && expectedType.equals("Attribute")) return true;
		if (type.equals("Attribute11") && expectedType.equals("Attribute")) return true;
		if (type.equals("Symbol6") && expectedType.equals("Symbol")) return true;
		if (type.equals("Symbol2") && expectedType.equals("Symbol")) return true;
		if (type.equals("Symbol1") && expectedType.equals("Symbol")) return true;
		if (type.equals("Term3") && expectedType.equals("Term")) return true;
		if (type.equals("Disambiguations2") && expectedType.equals("Disambiguations")) return true;
		if (type.equals("Sort4") && expectedType.equals("Sort")) return true;
		if (type.equals("ProdPart5") && expectedType.equals("ProdPart")) return true;
		if (type.equals("Attribute3") && expectedType.equals("Attribute")) return true;
		if (type.equals("Attribute7") && expectedType.equals("Attribute")) return true;
		if (type.equals("Priority1") && expectedType.equals("Priority")) return true;
		if (type.equals("Strategy3") && expectedType.equals("Strategy")) return true;
		if (type.equals("ModuleSort1") && expectedType.equals("ModuleSort")) return true;
		if (type.equals("ProdPart1") && expectedType.equals("ProdPart")) return true;
		if (type.equals("Attribute10") && expectedType.equals("Attribute")) return true;
		if (type.equals("Symbol3") && expectedType.equals("Symbol")) return true;
		if (type.equals("Grammer4") && expectedType.equals("Grammer")) return true;
		if (type.equals("Sort1") && expectedType.equals("Sort")) return true;
		if (type.equals("Disambiguations1") && expectedType.equals("Disambiguations")) return true;
		if (type.equals("Term2") && expectedType.equals("Term")) return true;
		if (type.equals("Sort5") && expectedType.equals("Sort")) return true;
		if (type.equals("RuleCond1") && expectedType.equals("RuleCond")) return true;
		if (type.equals("Sort2") && expectedType.equals("Sort")) return true;
		if (type.equals("SortOp3") && expectedType.equals("SortOp")) return true;
		if (type.equals("ProdPart2") && expectedType.equals("ProdPart")) return true;
		if (type.equals("Priority2") && expectedType.equals("Priority")) return true;
		if (type.equals("Constructor1") && expectedType.equals("Constructor")) return true;
		if (type.equals("Attribute4") && expectedType.equals("Attribute")) return true;
		if (type.equals("Grammer3") && expectedType.equals("Grammer")) return true;
		if (type.equals("Restriction2") && expectedType.equals("Restriction")) return true;
		if (type.equals("Strategy2") && expectedType.equals("Strategy")) return true;
		if (type.equals("Symbol4") && expectedType.equals("Symbol")) return true;
		if (type.equals("Term5") && expectedType.equals("Term")) return true;
		if (type.equals("ModuleSort2") && expectedType.equals("ModuleSort")) return true;
		if (type.equals("Term1") && expectedType.equals("Term")) return true;
		if (type.equals("Attribute8") && expectedType.equals("Attribute")) return true;
		if (type.equals("Attribute5") && expectedType.equals("Attribute")) return true;
		if (type.equals("RuleCond2") && expectedType.equals("RuleCond")) return true;
		if (type.equals("Attribute1") && expectedType.equals("Attribute")) return true;
		if (type.equals("SortOp2") && expectedType.equals("SortOp")) return true;
		if (type.equals("Constructor2") && expectedType.equals("Constructor")) return true;
		if (type.equals("Priority3") && expectedType.equals("Priority")) return true;
		if (type.equals("ProdPart3") && expectedType.equals("ProdPart")) return true;
		if (type.equals("Restriction1") && expectedType.equals("Restriction")) return true;
		if (type.equals("Grammer2") && expectedType.equals("Grammer")) return true;
		if (type.equals("Strategy1") && expectedType.equals("Strategy")) return true;
		if (type.equals("Term4") && expectedType.equals("Term")) return true;
		if (type.equals("Disambiguations3") && expectedType.equals("Disambiguations")) return true;
		if (type.equals("Sort3") && expectedType.equals("Sort")) return true;
		if (type.equals("Attribute9") && expectedType.equals("Attribute")) return true;
		if (type.equals("Symbol5") && expectedType.equals("Symbol")) return true;
		return false;
	}
}
