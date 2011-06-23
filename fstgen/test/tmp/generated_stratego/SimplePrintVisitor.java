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
		if (type.equals("Rule2") && expectedType.equals("Rule")) return true;
		if (type.equals("Anno3") && expectedType.equals("Anno")) return true;
		if (type.equals("Strategy16") && expectedType.equals("Strategy")) return true;
		if (type.equals("RuleDec3") && expectedType.equals("RuleDec")) return true;
		if (type.equals("DynRuleScopeId2") && expectedType.equals("DynRuleScopeId")) return true;
		if (type.equals("Decl4") && expectedType.equals("Decl")) return true;
		if (type.equals("Strategy9") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy26") && expectedType.equals("Strategy")) return true;
		if (type.equals("ModuleContent1") && expectedType.equals("ModuleContent")) return true;
		if (type.equals("Opdecl3") && expectedType.equals("Opdecl")) return true;
		if (type.equals("Sorts3") && expectedType.equals("Sorts")) return true;
		if (type.equals("Strategy46") && expectedType.equals("Strategy")) return true;
		if (type.equals("Term7") && expectedType.equals("Term")) return true;
		if (type.equals("Overlay1") && expectedType.equals("Overlay")) return true;
		if (type.equals("Strategy41") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy21") && expectedType.equals("Strategy")) return true;
		if (type.equals("PreTerm7") && expectedType.equals("PreTerm")) return true;
		if (type.equals("DynRuleDef2") && expectedType.equals("DynRuleDef")) return true;
		if (type.equals("Anno2") && expectedType.equals("Anno")) return true;
		if (type.equals("RuleDef3") && expectedType.equals("RuleDef")) return true;
		if (type.equals("Strategy31") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy36") && expectedType.equals("Strategy")) return true;
		if (type.equals("RuleDec2") && expectedType.equals("RuleDec")) return true;
		if (type.equals("Rule1") && expectedType.equals("Rule")) return true;
		if (type.equals("Strategy30") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy27") && expectedType.equals("Strategy")) return true;
		if (type.equals("Decl5") && expectedType.equals("Decl")) return true;
		if (type.equals("Strategy8") && expectedType.equals("Strategy")) return true;
		if (type.equals("DynRuleScopeId1") && expectedType.equals("DynRuleScopeId")) return true;
		if (type.equals("Strategy17") && expectedType.equals("Strategy")) return true;
		if (type.equals("Sorts2") && expectedType.equals("Sorts")) return true;
		if (type.equals("Term6") && expectedType.equals("Term")) return true;
		if (type.equals("Strategy40") && expectedType.equals("Strategy")) return true;
		if (type.equals("Overlay2") && expectedType.equals("Overlay")) return true;
		if (type.equals("Strategy47") && expectedType.equals("Strategy")) return true;
		if (type.equals("DynRuleDef1") && expectedType.equals("DynRuleDef")) return true;
		if (type.equals("PreTerm6") && expectedType.equals("PreTerm")) return true;
		if (type.equals("Strategy7") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy20") && expectedType.equals("Strategy")) return true;
		if (type.equals("RuleDef2") && expectedType.equals("RuleDef")) return true;
		if (type.equals("Anno1") && expectedType.equals("Anno")) return true;
		if (type.equals("Strategy37") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy14") && expectedType.equals("Strategy")) return true;
		if (type.equals("RuleCond1") && expectedType.equals("RuleCond")) return true;
		if (type.equals("Strategy28") && expectedType.equals("Strategy")) return true;
		if (type.equals("Sdecl2") && expectedType.equals("Sdecl")) return true;
		if (type.equals("Decl2") && expectedType.equals("Decl")) return true;
		if (type.equals("Opdecl1") && expectedType.equals("Opdecl")) return true;
		if (type.equals("DynRuleDef5") && expectedType.equals("DynRuleDef")) return true;
		if (type.equals("Strategy6") && expectedType.equals("Strategy")) return true;
		if (type.equals("Type2") && expectedType.equals("Type")) return true;
		if (type.equals("DynRuleDef4") && expectedType.equals("DynRuleDef")) return true;
		if (type.equals("RuleDef1") && expectedType.equals("RuleDef")) return true;
		if (type.equals("ScopeLabels2") && expectedType.equals("ScopeLabels")) return true;
		if (type.equals("StrategyDef6") && expectedType.equals("StrategyDef")) return true;
		if (type.equals("Strategy48") && expectedType.equals("Strategy")) return true;
		if (type.equals("Term5") && expectedType.equals("Term")) return true;
		if (type.equals("Def3") && expectedType.equals("Def")) return true;
		if (type.equals("Strategy33") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy34") && expectedType.equals("Strategy")) return true;
		if (type.equals("PreTerm1") && expectedType.equals("PreTerm")) return true;
		if (type.equals("Strategy15") && expectedType.equals("Strategy")) return true;
		if (type.equals("RuleCond2") && expectedType.equals("RuleCond")) return true;
		if (type.equals("Strategy29") && expectedType.equals("Strategy")) return true;
		if (type.equals("Opdecl2") && expectedType.equals("Opdecl")) return true;
		if (type.equals("ModuleContent2") && expectedType.equals("ModuleContent")) return true;
		if (type.equals("Decl3") && expectedType.equals("Decl")) return true;
		if (type.equals("Strategy5") && expectedType.equals("Strategy")) return true;
		if (type.equals("Term4") && expectedType.equals("Term")) return true;
		if (type.equals("Type1") && expectedType.equals("Type")) return true;
		if (type.equals("DynRuleDef3") && expectedType.equals("DynRuleDef")) return true;
		if (type.equals("ScopeLabels1") && expectedType.equals("ScopeLabels")) return true;
		if (type.equals("StrategyDef5") && expectedType.equals("StrategyDef")) return true;
		if (type.equals("Def4") && expectedType.equals("Def")) return true;
		if (type.equals("Strategy35") && expectedType.equals("Strategy")) return true;
		if (type.equals("PreTerm8") && expectedType.equals("PreTerm")) return true;
		if (type.equals("Strategy32") && expectedType.equals("Strategy")) return true;
		if (type.equals("DynRuleDef7") && expectedType.equals("DynRuleDef")) return true;
		if (type.equals("Def2") && expectedType.equals("Def")) return true;
		if (type.equals("RuleNames2") && expectedType.equals("RuleNames")) return true;
		if (type.equals("Strategy22") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy45") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy12") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy4") && expectedType.equals("Strategy")) return true;
		if (type.equals("ArgType1") && expectedType.equals("ArgType")) return true;
		if (type.equals("PreTerm3") && expectedType.equals("PreTerm")) return true;
		if (type.equals("StrategyDef4") && expectedType.equals("StrategyDef")) return true;
		if (type.equals("Term3") && expectedType.equals("Term")) return true;
		if (type.equals("DynRuleDef6") && expectedType.equals("DynRuleDef")) return true;
		if (type.equals("Def1") && expectedType.equals("Def")) return true;
		if (type.equals("Strategy23") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy44") && expectedType.equals("Strategy")) return true;
		if (type.equals("StrategyDef2") && expectedType.equals("StrategyDef")) return true;
		if (type.equals("Strategy3") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy13") && expectedType.equals("Strategy")) return true;
		if (type.equals("DynRuleId3") && expectedType.equals("DynRuleId")) return true;
		if (type.equals("Decl1") && expectedType.equals("Decl")) return true;
		if (type.equals("StrategyDef3") && expectedType.equals("StrategyDef")) return true;
		if (type.equals("PreTerm2") && expectedType.equals("PreTerm")) return true;
		if (type.equals("Term2") && expectedType.equals("Term")) return true;
		if (type.equals("Term9") && expectedType.equals("Term")) return true;
		if (type.equals("StrategyDef1") && expectedType.equals("StrategyDef")) return true;
		if (type.equals("Strategy43") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy24") && expectedType.equals("Strategy")) return true;
		if (type.equals("Decl6") && expectedType.equals("Decl")) return true;
		if (type.equals("Strategy10") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy2") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy38") && expectedType.equals("Strategy")) return true;
		if (type.equals("DynRuleId2") && expectedType.equals("DynRuleId")) return true;
		if (type.equals("Sdecl1") && expectedType.equals("Sdecl")) return true;
		if (type.equals("Sorts1") && expectedType.equals("Sorts")) return true;
		if (type.equals("Strategy18") && expectedType.equals("Strategy")) return true;
		if (type.equals("PreTerm5") && expectedType.equals("PreTerm")) return true;
		if (type.equals("Term1") && expectedType.equals("Term")) return true;
		if (type.equals("Strategy42") && expectedType.equals("Strategy")) return true;
		if (type.equals("RuleNames1") && expectedType.equals("RuleNames")) return true;
		if (type.equals("DynRuleDef8") && expectedType.equals("DynRuleDef")) return true;
		if (type.equals("Strategy25") && expectedType.equals("Strategy")) return true;
		if (type.equals("ArgType2") && expectedType.equals("ArgType")) return true;
		if (type.equals("Strategy11") && expectedType.equals("Strategy")) return true;
		if (type.equals("Term8") && expectedType.equals("Term")) return true;
		if (type.equals("Strategy1") && expectedType.equals("Strategy")) return true;
		if (type.equals("DynRuleId1") && expectedType.equals("DynRuleId")) return true;
		if (type.equals("Strategy19") && expectedType.equals("Strategy")) return true;
		if (type.equals("PreTerm4") && expectedType.equals("PreTerm")) return true;
		if (type.equals("RuleDec1") && expectedType.equals("RuleDec")) return true;
		if (type.equals("Rule3") && expectedType.equals("Rule")) return true;
		if (type.equals("Strategy39") && expectedType.equals("Strategy")) return true;
		return false;
	}
}
