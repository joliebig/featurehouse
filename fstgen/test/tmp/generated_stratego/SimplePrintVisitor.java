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
		if (type.equals("Strategy16") && expectedType.equals("Strategy")) return true;
		if (type.equals("Declaration3") && expectedType.equals("Declaration")) return true;
		if (type.equals("Anno3") && expectedType.equals("Anno")) return true;
		if (type.equals("DynRuleScopeId2") && expectedType.equals("DynRuleScopeId")) return true;
		if (type.equals("Strategy9") && expectedType.equals("Strategy")) return true;
		if (type.equals("TermOperator1") && expectedType.equals("TermOperator")) return true;
		if (type.equals("RuleDecOperator1") && expectedType.equals("RuleDecOperator")) return true;
		if (type.equals("Strat1") && expectedType.equals("Strat")) return true;
		if (type.equals("Strategy21") && expectedType.equals("Strategy")) return true;
		if (type.equals("DynRuleDef2") && expectedType.equals("DynRuleDef")) return true;
		if (type.equals("Anno2") && expectedType.equals("Anno")) return true;
		if (type.equals("Declaration4") && expectedType.equals("Declaration")) return true;
		if (type.equals("Strategy8") && expectedType.equals("Strategy")) return true;
		if (type.equals("TermOperator2") && expectedType.equals("TermOperator")) return true;
		if (type.equals("Strategy17") && expectedType.equals("Strategy")) return true;
		if (type.equals("DynRuleScopeId1") && expectedType.equals("DynRuleScopeId")) return true;
		if (type.equals("AlternativeOperator4") && expectedType.equals("AlternativeOperator")) return true;
		if (type.equals("TrafoSort1") && expectedType.equals("TrafoSort")) return true;
		if (type.equals("DynRuleDef1") && expectedType.equals("DynRuleDef")) return true;
		if (type.equals("PreTerm6") && expectedType.equals("PreTerm")) return true;
		if (type.equals("Strategy20") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy7") && expectedType.equals("Strategy")) return true;
		if (type.equals("Anno1") && expectedType.equals("Anno")) return true;
		if (type.equals("Strategy14") && expectedType.equals("Strategy")) return true;
		if (type.equals("RuleCond1") && expectedType.equals("RuleCond")) return true;
		if (type.equals("OptRuleOrStrategy2") && expectedType.equals("OptRuleOrStrategy")) return true;
		if (type.equals("Declaration1") && expectedType.equals("Declaration")) return true;
		if (type.equals("TermOperator3") && expectedType.equals("TermOperator")) return true;
		if (type.equals("BracketSort1") && expectedType.equals("BracketSort")) return true;
		if (type.equals("DynRuleDef5") && expectedType.equals("DynRuleDef")) return true;
		if (type.equals("Strategy6") && expectedType.equals("Strategy")) return true;
		if (type.equals("DynRuleDef4") && expectedType.equals("DynRuleDef")) return true;
		if (type.equals("Type2") && expectedType.equals("Type")) return true;
		if (type.equals("RuleOrTerm2") && expectedType.equals("RuleOrTerm")) return true;
		if (type.equals("ScopeLabels2") && expectedType.equals("ScopeLabels")) return true;
		if (type.equals("Strat3") && expectedType.equals("Strat")) return true;
		if (type.equals("Term5") && expectedType.equals("Term")) return true;
		if (type.equals("OptOpDecl22") && expectedType.equals("OptOpDecl2")) return true;
		if (type.equals("Strategy15") && expectedType.equals("Strategy")) return true;
		if (type.equals("PreTerm1") && expectedType.equals("PreTerm")) return true;
		if (type.equals("RuleCond2") && expectedType.equals("RuleCond")) return true;
		if (type.equals("ModuleDeclaration1") && expectedType.equals("ModuleDeclaration")) return true;
		if (type.equals("SigDeclaration1") && expectedType.equals("SigDeclaration")) return true;
		if (type.equals("Declaration2") && expectedType.equals("Declaration")) return true;
		if (type.equals("Strategy5") && expectedType.equals("Strategy")) return true;
		if (type.equals("RuleOrTerm1") && expectedType.equals("RuleOrTerm")) return true;
		if (type.equals("Term4") && expectedType.equals("Term")) return true;
		if (type.equals("DynRuleDef3") && expectedType.equals("DynRuleDef")) return true;
		if (type.equals("Type1") && expectedType.equals("Type")) return true;
		if (type.equals("Strat2") && expectedType.equals("Strat")) return true;
		if (type.equals("ScopeLabels1") && expectedType.equals("ScopeLabels")) return true;
		if (type.equals("RuleDecOperator2") && expectedType.equals("RuleDecOperator")) return true;
		if (type.equals("OptOpDecl21") && expectedType.equals("OptOpDecl2")) return true;
		if (type.equals("ModuleDeclaration2") && expectedType.equals("ModuleDeclaration")) return true;
		if (type.equals("Def2") && expectedType.equals("Def")) return true;
		if (type.equals("RuleNames2") && expectedType.equals("RuleNames")) return true;
		if (type.equals("Strategy22") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy12") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy4") && expectedType.equals("Strategy")) return true;
		if (type.equals("AlternativeStrategy2") && expectedType.equals("AlternativeStrategy")) return true;
		if (type.equals("ArgType1") && expectedType.equals("ArgType")) return true;
		if (type.equals("BracketSort2") && expectedType.equals("BracketSort")) return true;
		if (type.equals("Strat5") && expectedType.equals("Strat")) return true;
		if (type.equals("PreTerm3") && expectedType.equals("PreTerm")) return true;
		if (type.equals("Term3") && expectedType.equals("Term")) return true;
		if (type.equals("Def1") && expectedType.equals("Def")) return true;
		if (type.equals("Strategy23") && expectedType.equals("Strategy")) return true;
		if (type.equals("StrategyDef2") && expectedType.equals("StrategyDef")) return true;
		if (type.equals("Strategy3") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strat4") && expectedType.equals("Strat")) return true;
		if (type.equals("OptRuleOrStrategy1") && expectedType.equals("OptRuleOrStrategy")) return true;
		if (type.equals("Strategy13") && expectedType.equals("Strategy")) return true;
		if (type.equals("AlternativeStrategy1") && expectedType.equals("AlternativeStrategy")) return true;
		if (type.equals("PreTerm2") && expectedType.equals("PreTerm")) return true;
		if (type.equals("Term2") && expectedType.equals("Term")) return true;
		if (type.equals("StrategyDef1") && expectedType.equals("StrategyDef")) return true;
		if (type.equals("AlternativeOperator3") && expectedType.equals("AlternativeOperator")) return true;
		if (type.equals("Strategy24") && expectedType.equals("Strategy")) return true;
		if (type.equals("OptPreTerm2") && expectedType.equals("OptPreTerm")) return true;
		if (type.equals("Declaration5") && expectedType.equals("Declaration")) return true;
		if (type.equals("Strategy10") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy2") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strat7") && expectedType.equals("Strat")) return true;
		if (type.equals("Strategy18") && expectedType.equals("Strategy")) return true;
		if (type.equals("Term1") && expectedType.equals("Term")) return true;
		if (type.equals("PreTerm5") && expectedType.equals("PreTerm")) return true;
		if (type.equals("TrafoSort2") && expectedType.equals("TrafoSort")) return true;
		if (type.equals("AlternativeOperator2") && expectedType.equals("AlternativeOperator")) return true;
		if (type.equals("RuleNames1") && expectedType.equals("RuleNames")) return true;
		if (type.equals("Strategy25") && expectedType.equals("Strategy")) return true;
		if (type.equals("Declaration6") && expectedType.equals("Declaration")) return true;
		if (type.equals("ArgType2") && expectedType.equals("ArgType")) return true;
		if (type.equals("OptPreTerm1") && expectedType.equals("OptPreTerm")) return true;
		if (type.equals("Strategy11") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strategy1") && expectedType.equals("Strategy")) return true;
		if (type.equals("Strat6") && expectedType.equals("Strat")) return true;
		if (type.equals("Strategy19") && expectedType.equals("Strategy")) return true;
		if (type.equals("SigDeclaration2") && expectedType.equals("SigDeclaration")) return true;
		if (type.equals("PreTerm4") && expectedType.equals("PreTerm")) return true;
		if (type.equals("AlternativeOperator1") && expectedType.equals("AlternativeOperator")) return true;
		return false;
	}
}
