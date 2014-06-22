package de.ovgu.cide.fstgen.parsers.generated_AsmetaL;

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
		if (nonTerminal.getType().equals("DefaultInitialization")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "DefaultCommand");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "Initialization");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Initializations")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"Initialization")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "DefaultInitialization");
				if (v!=null) {
					v.accept(this);
				}
			}
			for (FSTNode v : getChildren(nonTerminal,"Initialization")) {
				v.accept(this);
			}
			hintNewLine();
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("ModuleDeclaration")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "ID");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintNewLine();
			{
				FSTNode v=getChild(nonTerminal, "Header");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintNewLine();
			{
				FSTNode v=getChild(nonTerminal, "Body");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintNewLine();
			{
				FSTNode v=getChild(nonTerminal, "MainMacroDeclaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintNewLine();
			{
				FSTNode v=getChild(nonTerminal, "Initializations");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("CompilationUnit")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "AsmOrModule");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ModuleDeclaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Header")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"ImportClause")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "ExportClause");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "Signature");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Signature")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "SignatureCommand");
				if (v!=null) {
					v.accept(this);
				}
			}
			for (FSTNode v : getChildren(nonTerminal,"Domain")) {
				v.accept(this);
			}
			for (FSTNode v : getChildren(nonTerminal,"Function")) {
				v.accept(this);
			}
			hintNewLine();
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Body")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "DefinitionsCommand");
				if (v!=null) {
					v.accept(this);
				}
			}
			for (FSTNode v : getChildren(nonTerminal,"DomainDefinition")) {
				v.accept(this);
			}
			for (FSTNode v : getChildren(nonTerminal,"FunctionDefinition")) {
				v.accept(this);
			}
			for (FSTNode v : getChildren(nonTerminal,"RuleDeclaration")) {
				v.accept(this);
			}
			for (FSTNode v : getChildren(nonTerminal,"CTLSPECS")) {
				v.accept(this);
			}
			for (FSTNode v : getChildren(nonTerminal,"Invariant")) {
				v.accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Invariant")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "UnnamedInvariant");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		throw new RuntimeException("Unknown Non Terminal in FST "+nonTerminal);
	}
	protected boolean isSubtype(String type, String expectedType) {
		if (type.equals(expectedType)) return true;
		if (type.equals("Rule2") && expectedType.equals("Rule")) return true;
		if (type.equals("VariableBindingTerm2") && expectedType.equals("VariableBindingTerm")) return true;
		if (type.equals("BooleanTerm1") && expectedType.equals("BooleanTerm")) return true;
		if (type.equals("ConstantTerm4") && expectedType.equals("ConstantTerm")) return true;
		if (type.equals("TermOrSequenceTerm1") && expectedType.equals("TermOrSequenceTerm")) return true;
		if (type.equals("BasicRule9") && expectedType.equals("BasicRule")) return true;
		if (type.equals("TurboRule4") && expectedType.equals("TurboRule")) return true;
		if (type.equals("ReladditiveExpr6") && expectedType.equals("ReladditiveExpr")) return true;
		if (type.equals("TurboDerivedRule2") && expectedType.equals("TurboDerivedRule")) return true;
		if (type.equals("getDomainByID1") && expectedType.equals("getDomainByID")) return true;
		if (type.equals("ExtendedTerm7") && expectedType.equals("ExtendedTerm")) return true;
		if (type.equals("BasicRule1") && expectedType.equals("BasicRule")) return true;
		if (type.equals("TypeDomain5") && expectedType.equals("TypeDomain")) return true;
		if (type.equals("ConstantTerm5") && expectedType.equals("ConstantTerm")) return true;
		if (type.equals("FiniteQuantificationTerm3") && expectedType.equals("FiniteQuantificationTerm")) return true;
		if (type.equals("BooleanTerm2") && expectedType.equals("BooleanTerm")) return true;
		if (type.equals("Rule1") && expectedType.equals("Rule")) return true;
		if (type.equals("VariableBindingTerm1") && expectedType.equals("VariableBindingTerm")) return true;
		if (type.equals("ConstantTerm3") && expectedType.equals("ConstantTerm")) return true;
		if (type.equals("BasicFunction1") && expectedType.equals("BasicFunction")) return true;
		if (type.equals("TurboRule3") && expectedType.equals("TurboRule")) return true;
		if (type.equals("TurboDerivedRule1") && expectedType.equals("TurboDerivedRule")) return true;
		if (type.equals("ReladditiveExpr5") && expectedType.equals("ReladditiveExpr")) return true;
		if (type.equals("TermAsRule2") && expectedType.equals("TermAsRule")) return true;
		if (type.equals("BasicRule2") && expectedType.equals("BasicRule")) return true;
		if (type.equals("FiniteQuantificationTerm2") && expectedType.equals("FiniteQuantificationTerm")) return true;
		if (type.equals("TypeDomain4") && expectedType.equals("TypeDomain")) return true;
		if (type.equals("CollectionTerm1") && expectedType.equals("CollectionTerm")) return true;
		if (type.equals("unaryExpr2") && expectedType.equals("unaryExpr")) return true;
		if (type.equals("basicExpr1") && expectedType.equals("basicExpr")) return true;
		if (type.equals("ConstantTerm2") && expectedType.equals("ConstantTerm")) return true;
		if (type.equals("BasicFunction2") && expectedType.equals("BasicFunction")) return true;
		if (type.equals("BasicTerm3") && expectedType.equals("BasicTerm")) return true;
		if (type.equals("ExtendedTerm1") && expectedType.equals("ExtendedTerm")) return true;
		if (type.equals("BasicRule7") && expectedType.equals("BasicRule")) return true;
		if (type.equals("DynamicFunction5") && expectedType.equals("DynamicFunction")) return true;
		if (type.equals("TypeDomain3") && expectedType.equals("TypeDomain")) return true;
		if (type.equals("BXB_BExpression1") && expectedType.equals("BXB_BExpression")) return true;
		if (type.equals("TermAsRule1") && expectedType.equals("TermAsRule")) return true;
		if (type.equals("LocationOrVariableTerm1") && expectedType.equals("LocationOrVariableTerm")) return true;
		if (type.equals("basicExpr2") && expectedType.equals("basicExpr")) return true;
		if (type.equals("DomainOrFunctionOrRule1") && expectedType.equals("DomainOrFunctionOrRule")) return true;
		if (type.equals("ExtendedTerm2") && expectedType.equals("ExtendedTerm")) return true;
		if (type.equals("TermOrSequenceTerm2") && expectedType.equals("TermOrSequenceTerm")) return true;
		if (type.equals("ConstantTerm1") && expectedType.equals("ConstantTerm")) return true;
		if (type.equals("InvariantRefinement1") && expectedType.equals("InvariantRefinement")) return true;
		if (type.equals("getDomainByID2") && expectedType.equals("getDomainByID")) return true;
		if (type.equals("TypeDomain2") && expectedType.equals("TypeDomain")) return true;
		if (type.equals("BXB_BExpression7") && expectedType.equals("BXB_BExpression")) return true;
		if (type.equals("ArrowTermAdditionalArrowTerm1") && expectedType.equals("ArrowTermAdditionalArrowTerm")) return true;
		if (type.equals("BasicRule8") && expectedType.equals("BasicRule")) return true;
		if (type.equals("StructuredTD5") && expectedType.equals("StructuredTD")) return true;
		if (type.equals("signpowerExpr1") && expectedType.equals("signpowerExpr")) return true;
		if (type.equals("BXB_BExpression2") && expectedType.equals("BXB_BExpression")) return true;
		if (type.equals("B_BExpression2") && expectedType.equals("B_BExpression")) return true;
		if (type.equals("DomainOrFunctionIDOrRule3") && expectedType.equals("DomainOrFunctionIDOrRule")) return true;
		if (type.equals("basicExpr3") && expectedType.equals("basicExpr")) return true;
		if (type.equals("CollectionTerm3") && expectedType.equals("CollectionTerm")) return true;
		if (type.equals("BXB_BExpression6") && expectedType.equals("BXB_BExpression")) return true;
		if (type.equals("PlusMinusMultiExpr1") && expectedType.equals("PlusMinusMultiExpr")) return true;
		if (type.equals("DomainOrFunctionIDOrRule1") && expectedType.equals("DomainOrFunctionIDOrRule")) return true;
		if (type.equals("TypeDomain1") && expectedType.equals("TypeDomain")) return true;
		if (type.equals("DerivedRule2") && expectedType.equals("DerivedRule")) return true;
		if (type.equals("DynamicFunction4") && expectedType.equals("DynamicFunction")) return true;
		if (type.equals("ComprehensionTerm3") && expectedType.equals("ComprehensionTerm")) return true;
		if (type.equals("InvariantRefinement2") && expectedType.equals("InvariantRefinement")) return true;
		if (type.equals("signpowerExpr3") && expectedType.equals("signpowerExpr")) return true;
		if (type.equals("basicExpr4") && expectedType.equals("basicExpr")) return true;
		if (type.equals("ExportBodyOrAst2") && expectedType.equals("ExportBodyOrAst")) return true;
		if (type.equals("LocationOrVariableTerm2") && expectedType.equals("LocationOrVariableTerm")) return true;
		if (type.equals("B_BExpression1") && expectedType.equals("B_BExpression")) return true;
		if (type.equals("signunaryExpr2") && expectedType.equals("signunaryExpr")) return true;
		if (type.equals("ConstantTerm9") && expectedType.equals("ConstantTerm")) return true;
		if (type.equals("BasicRule5") && expectedType.equals("BasicRule")) return true;
		if (type.equals("TurboCallRule2") && expectedType.equals("TurboCallRule")) return true;
		if (type.equals("ReladditiveExpr2") && expectedType.equals("ReladditiveExpr")) return true;
		if (type.equals("AsmOrModule1") && expectedType.equals("AsmOrModule")) return true;
		if (type.equals("StructuredTD4") && expectedType.equals("StructuredTD")) return true;
		if (type.equals("ExtendedTerm3") && expectedType.equals("ExtendedTerm")) return true;
		if (type.equals("Rule6") && expectedType.equals("Rule")) return true;
		if (type.equals("ExportBodyOrAst1") && expectedType.equals("ExportBodyOrAst")) return true;
		if (type.equals("DomainOrFunctionIDOrRule2") && expectedType.equals("DomainOrFunctionIDOrRule")) return true;
		if (type.equals("DerivedRule1") && expectedType.equals("DerivedRule")) return true;
		if (type.equals("BXB_BExpression5") && expectedType.equals("BXB_BExpression")) return true;
		if (type.equals("CollectionTerm2") && expectedType.equals("CollectionTerm")) return true;
		if (type.equals("PlusMinusMultiExpr2") && expectedType.equals("PlusMinusMultiExpr")) return true;
		if (type.equals("signpowerExpr2") && expectedType.equals("signpowerExpr")) return true;
		if (type.equals("basicExpr5") && expectedType.equals("basicExpr")) return true;
		if (type.equals("ComprehensionTerm4") && expectedType.equals("ComprehensionTerm")) return true;
		if (type.equals("InvariantRefinement3") && expectedType.equals("InvariantRefinement")) return true;
		if (type.equals("DynamicFunction3") && expectedType.equals("DynamicFunction")) return true;
		if (type.equals("ConstantTerm8") && expectedType.equals("ConstantTerm")) return true;
		if (type.equals("signunaryExpr1") && expectedType.equals("signunaryExpr")) return true;
		if (type.equals("BasicRule6") && expectedType.equals("BasicRule")) return true;
		if (type.equals("ArrowTermAdditionalArrowTerm2") && expectedType.equals("ArrowTermAdditionalArrowTerm")) return true;
		if (type.equals("RuleDeclaration") && expectedType.equals("MainMacroDeclaration")) return true;
		if (type.equals("ReladditiveExpr1") && expectedType.equals("ReladditiveExpr")) return true;
		if (type.equals("TurboCallRule1") && expectedType.equals("TurboCallRule")) return true;
		if (type.equals("Rule5") && expectedType.equals("Rule")) return true;
		if (type.equals("Term2") && expectedType.equals("Term")) return true;
		if (type.equals("StructuredTD3") && expectedType.equals("StructuredTD")) return true;
		if (type.equals("ExtendedTerm4") && expectedType.equals("ExtendedTerm")) return true;
		if (type.equals("AsmOrModule2") && expectedType.equals("AsmOrModule")) return true;
		if (type.equals("BXB_BExpression4") && expectedType.equals("BXB_BExpression")) return true;
		if (type.equals("InvariantRefinement4") && expectedType.equals("InvariantRefinement")) return true;
		if (type.equals("TurboRule2") && expectedType.equals("TurboRule")) return true;
		if (type.equals("DynamicFunction2") && expectedType.equals("DynamicFunction")) return true;
		if (type.equals("DomainOrFunctionOrRule2") && expectedType.equals("DomainOrFunctionOrRule")) return true;
		if (type.equals("FiniteQuantificationTerm1") && expectedType.equals("FiniteQuantificationTerm")) return true;
		if (type.equals("ComprehensionTerm1") && expectedType.equals("ComprehensionTerm")) return true;
		if (type.equals("BasicRule3") && expectedType.equals("BasicRule")) return true;
		if (type.equals("ReladditiveExpr4") && expectedType.equals("ReladditiveExpr")) return true;
		if (type.equals("unaryExpr1") && expectedType.equals("unaryExpr")) return true;
		if (type.equals("BasicTerm2") && expectedType.equals("BasicTerm")) return true;
		if (type.equals("ConstantTerm7") && expectedType.equals("ConstantTerm")) return true;
		if (type.equals("Term1") && expectedType.equals("Term")) return true;
		if (type.equals("StructuredTD2") && expectedType.equals("StructuredTD")) return true;
		if (type.equals("ExtendedTerm5") && expectedType.equals("ExtendedTerm")) return true;
		if (type.equals("Rule4") && expectedType.equals("Rule")) return true;
		if (type.equals("BXB_BExpression3") && expectedType.equals("BXB_BExpression")) return true;
		if (type.equals("CollectionTerm4") && expectedType.equals("CollectionTerm")) return true;
		if (type.equals("TurboRule1") && expectedType.equals("TurboRule")) return true;
		if (type.equals("DynamicFunction1") && expectedType.equals("DynamicFunction")) return true;
		if (type.equals("ComprehensionTerm2") && expectedType.equals("ComprehensionTerm")) return true;
		if (type.equals("VariableBindingTerm3") && expectedType.equals("VariableBindingTerm")) return true;
		if (type.equals("DomainOrFunctionOrRule3") && expectedType.equals("DomainOrFunctionOrRule")) return true;
		if (type.equals("BasicRule4") && expectedType.equals("BasicRule")) return true;
		if (type.equals("ReladditiveExpr3") && expectedType.equals("ReladditiveExpr")) return true;
		if (type.equals("BasicTerm1") && expectedType.equals("BasicTerm")) return true;
		if (type.equals("ConstantTerm6") && expectedType.equals("ConstantTerm")) return true;
		if (type.equals("Rule3") && expectedType.equals("Rule")) return true;
		if (type.equals("StructuredTD1") && expectedType.equals("StructuredTD")) return true;
		if (type.equals("ExtendedTerm6") && expectedType.equals("ExtendedTerm")) return true;
		return false;
	}
}
