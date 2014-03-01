package de.ovgu.cide.fstgen.parsers.generated_javacc;

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
		if (nonTerminal.getType().equals("javacc_input")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "javacc_options");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("PARSER_BEGIN");
			printToken("(");
			{
				FSTNode v=getChild(nonTerminal, "ParserNameS");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken(")");
			{
				FSTNode v=getChild(nonTerminal, "CompilationUnit");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("PARSER_END");
			printToken("(");
			{
				FSTNode v=getChild(nonTerminal, "ParserNameE");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken(")");
			for (FSTNode v : getChildren(nonTerminal,"production")) {
				v.accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("javacc_options")) {
			printFeatures(nonTerminal,true);
			printToken("options");
			printToken("{");
			for (FSTNode v : getChildren(nonTerminal,"option_binding")) {
				v.accept(this);
			}
			printToken("}");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("RegExprProduction")) {
			printFeatures(nonTerminal,true);
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "regexpr_spec").iterator();
			{
				FSTNode v=getChild(nonTerminal, "regular_expr_productionPrefix");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "regexpr_kind");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken(":");
			printToken("{");
			hintIncIndent();
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				hintNewLine();
				printToken("|");
				listElements.next().accept(this);
			}
			hintDecIndent();
			printToken("}");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("BNFProduction")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "AccessModifier");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ResultType");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "BNFName");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "FormalParameters");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ThrowsClause");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken(":");
			{
				FSTNode v=getChild(nonTerminal, "Block");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("{");
			hintIncIndent();
			{
				FSTNode v=getChild(nonTerminal, "expansion_choices");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintDecIndent();
			printToken("}");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("expansion_choices")) {
			printFeatures(nonTerminal,true);
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "expansion").iterator();
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				hintNewLine();
				printToken("|");
				listElements.next().accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("expansion")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "local_lookahead");
				if (v!=null) {
					printToken("LOOKAHEAD");
					printToken("(");
					v.accept(this);
					printToken(")");
				}
			}
			for (FSTNode v : getChildren(nonTerminal,"expansion_unit")) {
				v.accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		throw new RuntimeException("Unknown Non Terminal in FST "+nonTerminal);
	}
	protected boolean isSubtype(String type, String expectedType) {
		if (type.equals(expectedType)) return true;
		if (type.equals("CREUPostfix3") && expectedType.equals("CREUPostfix")) return true;
		if (type.equals("TypeArgument1") && expectedType.equals("TypeArgument")) return true;
		if (type.equals("OptionName4") && expectedType.equals("OptionName")) return true;
		if (type.equals("PrimitiveType5") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("ExplicitConstructorInvocation1") && expectedType.equals("ExplicitConstructorInvocation")) return true;
		if (type.equals("StatementExpressionAssignment2") && expectedType.equals("StatementExpressionAssignment")) return true;
		if (type.equals("AssignmentOperator2") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("RelationalOp2") && expectedType.equals("RelationalOp")) return true;
		if (type.equals("ClassOrInterfaceBodyDeclaration7") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("regexpr_kind3") && expectedType.equals("regexpr_kind")) return true;
		if (type.equals("JavaIdentifier2") && expectedType.equals("JavaIdentifier")) return true;
		if (type.equals("Statement3") && expectedType.equals("Statement")) return true;
		if (type.equals("CastLAOp3") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("regular_expr_productionPrefix2") && expectedType.equals("regular_expr_productionPrefix")) return true;
		if (type.equals("CREUPostfix4") && expectedType.equals("CREUPostfix")) return true;
		if (type.equals("regular_expression3") && expectedType.equals("regular_expression")) return true;
		if (type.equals("Literal1") && expectedType.equals("Literal")) return true;
		if (type.equals("StatementExpressionAssignment3") && expectedType.equals("StatementExpressionAssignment")) return true;
		if (type.equals("PrimitiveType6") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("AssignmentOperator3") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("RelationalOp3") && expectedType.equals("RelationalOp")) return true;
		if (type.equals("PrimarySuffix6") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("Statement16") && expectedType.equals("Statement")) return true;
		if (type.equals("TryStatementEnd1") && expectedType.equals("TryStatementEnd")) return true;
		if (type.equals("JavaIdentifier3") && expectedType.equals("JavaIdentifier")) return true;
		if (type.equals("TypeDeclaration4") && expectedType.equals("TypeDeclaration")) return true;
		if (type.equals("ClassOrInterfaceBodyDeclaration6") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("regexpr_kind2") && expectedType.equals("regexpr_kind")) return true;
		if (type.equals("AllocationExpression2") && expectedType.equals("AllocationExpression")) return true;
		if (type.equals("Statement2") && expectedType.equals("Statement")) return true;
		if (type.equals("regular_expr_productionPrefix1") && expectedType.equals("regular_expr_productionPrefix")) return true;
		if (type.equals("CastLAOp4") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("OptionName2") && expectedType.equals("OptionName")) return true;
		if (type.equals("regular_expression2") && expectedType.equals("regular_expression")) return true;
		if (type.equals("PrimitiveType7") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("Type2") && expectedType.equals("Type")) return true;
		if (type.equals("ClassOrInterfaceBodyDeclaration5") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("regexpr_kind1") && expectedType.equals("regexpr_kind")) return true;
		if (type.equals("TypeDeclaration3") && expectedType.equals("TypeDeclaration")) return true;
		if (type.equals("BNFProduction") && expectedType.equals("production")) return true;
		if (type.equals("PrimarySuffix5") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("PrimaryPrefix1") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("CastLAOp1") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("JavaIdentifier4") && expectedType.equals("JavaIdentifier")) return true;
		if (type.equals("Statement1") && expectedType.equals("Statement")) return true;
		if (type.equals("OptionName3") && expectedType.equals("OptionName")) return true;
		if (type.equals("regular_expression1") && expectedType.equals("regular_expression")) return true;
		if (type.equals("TypeArgument2") && expectedType.equals("TypeArgument")) return true;
		if (type.equals("RelationalOp1") && expectedType.equals("RelationalOp")) return true;
		if (type.equals("PrimarySuffix3") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("PrimitiveType8") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("AssignmentOperator1") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("StatementExpressionAssignment1") && expectedType.equals("StatementExpressionAssignment")) return true;
		if (type.equals("TypeDeclaration2") && expectedType.equals("TypeDeclaration")) return true;
		if (type.equals("ClassOrInterfaceBodyDeclaration4") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("Type1") && expectedType.equals("Type")) return true;
		if (type.equals("JavaIdentifier5") && expectedType.equals("JavaIdentifier")) return true;
		if (type.equals("PrimaryPrefix2") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("PrimarySuffix4") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("CastLAOp2") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("VariableInitializer1") && expectedType.equals("VariableInitializer")) return true;
		if (type.equals("MultiplicativeOp1") && expectedType.equals("MultiplicativeOp")) return true;
		if (type.equals("StatementExpression1") && expectedType.equals("StatementExpression")) return true;
		if (type.equals("Modifier10") && expectedType.equals("Modifier")) return true;
		if (type.equals("ClassOrInterface2") && expectedType.equals("ClassOrInterface")) return true;
		if (type.equals("ForStatementInternal2") && expectedType.equals("ForStatementInternal")) return true;
		if (type.equals("OptionValue1") && expectedType.equals("OptionValue")) return true;
		if (type.equals("Statement7") && expectedType.equals("Statement")) return true;
		if (type.equals("JavaIdentifier6") && expectedType.equals("JavaIdentifier")) return true;
		if (type.equals("AnnotationTypeMemberDeclaration5") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("PrimitiveType1") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("UnaryExpressionNotPlusMinus2") && expectedType.equals("UnaryExpressionNotPlusMinus")) return true;
		if (type.equals("ClassOrInterfaceBodyDeclaration3") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("PrimitiveType2") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("VariableInitializer2") && expectedType.equals("VariableInitializer")) return true;
		if (type.equals("ClassOrInterface1") && expectedType.equals("ClassOrInterface")) return true;
		if (type.equals("UnaryExpressionNotPlusMinus3") && expectedType.equals("UnaryExpressionNotPlusMinus")) return true;
		if (type.equals("ForStatementInternal1") && expectedType.equals("ForStatementInternal")) return true;
		if (type.equals("ClassOrInterfaceBodyDeclaration2") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("CastLookahead3") && expectedType.equals("CastLookahead")) return true;
		if (type.equals("Statement6") && expectedType.equals("Statement")) return true;
		if (type.equals("OptionValue2") && expectedType.equals("OptionValue")) return true;
		if (type.equals("JavaIdentifier7") && expectedType.equals("JavaIdentifier")) return true;
		if (type.equals("AnnotationTypeMemberDeclaration6") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("StatementExpression3") && expectedType.equals("StatementExpression")) return true;
		if (type.equals("RelationalOp4") && expectedType.equals("RelationalOp")) return true;
		if (type.equals("Annotation1") && expectedType.equals("Annotation")) return true;
		if (type.equals("PrimitiveType3") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("ConditionalExpression2") && expectedType.equals("ConditionalExpression")) return true;
		if (type.equals("MethodDeclarationBody1") && expectedType.equals("MethodDeclarationBody")) return true;
		if (type.equals("Modifier12") && expectedType.equals("Modifier")) return true;
		if (type.equals("Statement5") && expectedType.equals("Statement")) return true;
		if (type.equals("JavaIdentifier8") && expectedType.equals("JavaIdentifier")) return true;
		if (type.equals("OptionValue3") && expectedType.equals("OptionValue")) return true;
		if (type.equals("ClassOrInterfaceBodyDeclaration1") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("AllocationExpression1") && expectedType.equals("AllocationExpression")) return true;
		if (type.equals("expansion_unitMain2") && expectedType.equals("expansion_unitMain")) return true;
		if (type.equals("AnnotationTypeMemberDeclaration3") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("StatementExpression2") && expectedType.equals("StatementExpression")) return true;
		if (type.equals("Annotation2") && expectedType.equals("Annotation")) return true;
		if (type.equals("RegExprProduction") && expectedType.equals("production")) return true;
		if (type.equals("PrimitiveType4") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("ExplicitConstructorInvocation2") && expectedType.equals("ExplicitConstructorInvocation")) return true;
		if (type.equals("expansion_unitMain1") && expectedType.equals("expansion_unitMain")) return true;
		if (type.equals("ConditionalExpression1") && expectedType.equals("ConditionalExpression")) return true;
		if (type.equals("Modifier11") && expectedType.equals("Modifier")) return true;
		if (type.equals("Modifier1") && expectedType.equals("Modifier")) return true;
		if (type.equals("JavaIdentifier9") && expectedType.equals("JavaIdentifier")) return true;
		if (type.equals("Statement4") && expectedType.equals("Statement")) return true;
		if (type.equals("BooleanLiteral2") && expectedType.equals("BooleanLiteral")) return true;
		if (type.equals("UnaryExpressionNotPlusMinus1") && expectedType.equals("UnaryExpressionNotPlusMinus")) return true;
		if (type.equals("AnnotationTypeMemberDeclaration4") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("AllocationExpressionInit1") && expectedType.equals("AllocationExpressionInit")) return true;
		if (type.equals("AnnotationTypeMemberDeclaration1") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("ReferenceTypeP2") && expectedType.equals("ReferenceTypeP")) return true;
		if (type.equals("AssignmentOperator10") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("Statement10") && expectedType.equals("Statement")) return true;
		if (type.equals("Modifier2") && expectedType.equals("Modifier")) return true;
		if (type.equals("ArrayDimsAndInits1") && expectedType.equals("ArrayDimsAndInits")) return true;
		if (type.equals("ForInit1") && expectedType.equals("ForInit")) return true;
		if (type.equals("ExpModifier1") && expectedType.equals("ExpModifier")) return true;
		if (type.equals("CastExpression2") && expectedType.equals("CastExpression")) return true;
		if (type.equals("JavaIdentifier13") && expectedType.equals("JavaIdentifier")) return true;
		if (type.equals("JavaIdentifier1") && expectedType.equals("JavaIdentifier")) return true;
		if (type.equals("Annotation3") && expectedType.equals("Annotation")) return true;
		if (type.equals("UnaryExpression3") && expectedType.equals("UnaryExpression")) return true;
		if (type.equals("expansion_unit4") && expectedType.equals("expansion_unit")) return true;
		if (type.equals("PostfixOp1") && expectedType.equals("PostfixOp")) return true;
		if (type.equals("AllocationExpressionInit2") && expectedType.equals("AllocationExpressionInit")) return true;
		if (type.equals("AnnotationTypeMemberDeclaration2") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("ReferenceTypeP1") && expectedType.equals("ReferenceTypeP")) return true;
		if (type.equals("SwitchLabel2") && expectedType.equals("SwitchLabel")) return true;
		if (type.equals("AssignmentOperator9") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("Statement11") && expectedType.equals("Statement")) return true;
		if (type.equals("Modifier3") && expectedType.equals("Modifier")) return true;
		if (type.equals("ArrayDimsAndInits2") && expectedType.equals("ArrayDimsAndInits")) return true;
		if (type.equals("JavaIdentifier12") && expectedType.equals("JavaIdentifier")) return true;
		if (type.equals("CastExpression1") && expectedType.equals("CastExpression")) return true;
		if (type.equals("PostfixOp2") && expectedType.equals("PostfixOp")) return true;
		if (type.equals("UnaryExpression2") && expectedType.equals("UnaryExpression")) return true;
		if (type.equals("ForInit2") && expectedType.equals("ForInit")) return true;
		if (type.equals("expansion_unit5") && expectedType.equals("expansion_unit")) return true;
		if (type.equals("SwitchLabel1") && expectedType.equals("SwitchLabel")) return true;
		if (type.equals("Modifier4") && expectedType.equals("Modifier")) return true;
		if (type.equals("Statement9") && expectedType.equals("Statement")) return true;
		if (type.equals("ShiftOp2") && expectedType.equals("ShiftOp")) return true;
		if (type.equals("AdditiveOp1") && expectedType.equals("AdditiveOp")) return true;
		if (type.equals("CastLookahead2") && expectedType.equals("CastLookahead")) return true;
		if (type.equals("EqualityOp1") && expectedType.equals("EqualityOp")) return true;
		if (type.equals("AdditiveOp2") && expectedType.equals("AdditiveOp")) return true;
		if (type.equals("ExpModifier3") && expectedType.equals("ExpModifier")) return true;
		if (type.equals("JavaIdentifier11") && expectedType.equals("JavaIdentifier")) return true;
		if (type.equals("MemberValue1") && expectedType.equals("MemberValue")) return true;
		if (type.equals("expansion_unit6") && expectedType.equals("expansion_unit")) return true;
		if (type.equals("MultiplicativeOp3") && expectedType.equals("MultiplicativeOp")) return true;
		if (type.equals("Literal6") && expectedType.equals("Literal")) return true;
		if (type.equals("AssignmentOperator12") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("MethodDeclarationBody2") && expectedType.equals("MethodDeclarationBody")) return true;
		if (type.equals("UnaryOp2") && expectedType.equals("UnaryOp")) return true;
		if (type.equals("Statement8") && expectedType.equals("Statement")) return true;
		if (type.equals("Modifier5") && expectedType.equals("Modifier")) return true;
		if (type.equals("ShiftOp1") && expectedType.equals("ShiftOp")) return true;
		if (type.equals("CastLookahead1") && expectedType.equals("CastLookahead")) return true;
		if (type.equals("EqualityOp2") && expectedType.equals("EqualityOp")) return true;
		if (type.equals("ExpModifier2") && expectedType.equals("ExpModifier")) return true;
		if (type.equals("FinallyBlock") && expectedType.equals("TryStatementEnd")) return true;
		if (type.equals("JavaIdentifier10") && expectedType.equals("JavaIdentifier")) return true;
		if (type.equals("MemberValue2") && expectedType.equals("MemberValue")) return true;
		if (type.equals("UnaryExpression4") && expectedType.equals("UnaryExpression")) return true;
		if (type.equals("MultiplicativeOp2") && expectedType.equals("MultiplicativeOp")) return true;
		if (type.equals("AssignmentOperator11") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("AssignmentOperator6") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("BlockStatement3") && expectedType.equals("BlockStatement")) return true;
		if (type.equals("CastLAOp7") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("WildcardBounds1") && expectedType.equals("WildcardBounds")) return true;
		if (type.equals("Statement14") && expectedType.equals("Statement")) return true;
		if (type.equals("complex_regular_expression_unit4") && expectedType.equals("complex_regular_expression_unit")) return true;
		if (type.equals("Modifier6") && expectedType.equals("Modifier")) return true;
		if (type.equals("UnaryOp1") && expectedType.equals("UnaryOp")) return true;
		if (type.equals("PrimarySuffix2") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("Literal4") && expectedType.equals("Literal")) return true;
		if (type.equals("MemberValue3") && expectedType.equals("MemberValue")) return true;
		if (type.equals("AssignmentOperator5") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("PrimaryPrefix7") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("CastLAOp8") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("WildcardBounds2") && expectedType.equals("WildcardBounds")) return true;
		if (type.equals("Statement15") && expectedType.equals("Statement")) return true;
		if (type.equals("ShiftOp3") && expectedType.equals("ShiftOp")) return true;
		if (type.equals("BlockStatement2") && expectedType.equals("BlockStatement")) return true;
		if (type.equals("complex_regular_expression_unit3") && expectedType.equals("complex_regular_expression_unit")) return true;
		if (type.equals("Modifier7") && expectedType.equals("Modifier")) return true;
		if (type.equals("expansion_unit1") && expectedType.equals("expansion_unit")) return true;
		if (type.equals("PrimarySuffix1") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("Literal5") && expectedType.equals("Literal")) return true;
		if (type.equals("production3") && expectedType.equals("production")) return true;
		if (type.equals("AssignmentOperator4") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("PrimaryPrefix6") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("BlockStatement1") && expectedType.equals("BlockStatement")) return true;
		if (type.equals("CastLAOp5") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("EOFExpr") && expectedType.equals("regular_expression")) return true;
		if (type.equals("AssignmentOperator8") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("PrimaryPrefix4") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("Statement12") && expectedType.equals("Statement")) return true;
		if (type.equals("Modifier8") && expectedType.equals("Modifier")) return true;
		if (type.equals("regexpr_kind4") && expectedType.equals("regexpr_kind")) return true;
		if (type.equals("complex_regular_expression_unit2") && expectedType.equals("complex_regular_expression_unit")) return true;
		if (type.equals("OptionName1") && expectedType.equals("OptionName")) return true;
		if (type.equals("Literal2") && expectedType.equals("Literal")) return true;
		if (type.equals("expansion_unit2") && expectedType.equals("expansion_unit")) return true;
		if (type.equals("UnaryExpression1") && expectedType.equals("UnaryExpression")) return true;
		if (type.equals("CREUPostfix1") && expectedType.equals("CREUPostfix")) return true;
		if (type.equals("PrimaryPrefix5") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("TypeDeclaration1") && expectedType.equals("TypeDeclaration")) return true;
		if (type.equals("complex_regular_expression_unit1") && expectedType.equals("complex_regular_expression_unit")) return true;
		if (type.equals("Statement13") && expectedType.equals("Statement")) return true;
		if (type.equals("CastLAOp6") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("PrimaryPrefix3") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("AssignmentOperator7") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("Modifier9") && expectedType.equals("Modifier")) return true;
		if (type.equals("expansion_unit3") && expectedType.equals("expansion_unit")) return true;
		if (type.equals("Literal3") && expectedType.equals("Literal")) return true;
		if (type.equals("BooleanLiteral1") && expectedType.equals("BooleanLiteral")) return true;
		if (type.equals("production1") && expectedType.equals("production")) return true;
		if (type.equals("CREUPostfix2") && expectedType.equals("CREUPostfix")) return true;
		return false;
	}
}
