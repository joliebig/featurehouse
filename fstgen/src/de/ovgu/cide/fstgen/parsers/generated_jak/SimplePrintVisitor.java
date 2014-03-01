package de.ovgu.cide.fstgen.parsers.generated_jak;

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
		if (nonTerminal.getType().equals("CompilationUnit")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "LayerDeclaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "PackageDeclaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			for (FSTNode v : getChildren(nonTerminal,"ImportDeclaration")) {
				v.accept(this);
			}
			for (FSTNode v : getChildren(nonTerminal,"TypeDeclaration")) {
				v.accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("AnnotationTypeDeclaration")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "Modifiers");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("@");
			printToken("interface");
			{
				FSTNode v=getChild(nonTerminal, "Id");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("{");
			for (FSTNode v : getChildren(nonTerminal,"AnnotationTypeMemberDeclaration")) {
				v.accept(this);
			}
			printToken("}");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("ClassDeclaration")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "Modifiers");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ClassOrInterface");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "Id");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "TypeParameters");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ExtendsList");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ImplementsList");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("{");
			hintIncIndent();
			hintNewLine();
			for (FSTNode v : getChildren(nonTerminal,"ClassOrInterfaceBodyDeclaration")) {
				v.accept(this);
				hintNewLine();
				hintNewLine();
			}
			hintDecIndent();
			hintNewLine();
			printToken("}");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("EnumDecl")) {
			printFeatures(nonTerminal,true);
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "EnumConstant").iterator();
			{
				FSTNode v=getChild(nonTerminal, "Modifiers");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("enum");
			{
				FSTNode v=getChild(nonTerminal, "Id");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ImplementsList");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("{");
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				printToken(",");
				listElements.next().accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "EnumBodyInternal");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("}");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("EnumBodyInternal")) {
			printFeatures(nonTerminal,true);
			printToken(";");
			for (FSTNode v : getChildren(nonTerminal,"ClassOrInterfaceBodyDeclaration")) {
				v.accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("InnerClassDecl")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "Modifiers");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ClassOrInterface");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "Id");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "TypeParameters");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ExtendsList");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ImplementsList");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("{");
			hintIncIndent();
			hintNewLine();
			for (FSTNode v : getChildren(nonTerminal,"ClassOrInterfaceBodyDeclaration")) {
				v.accept(this);
				hintNewLine();
				hintNewLine();
			}
			hintDecIndent();
			hintNewLine();
			printToken("}");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("InnerEnumDecl")) {
			printFeatures(nonTerminal,true);
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "EnumConstant").iterator();
			{
				FSTNode v=getChild(nonTerminal, "Modifiers");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("enum");
			{
				FSTNode v=getChild(nonTerminal, "Id");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ImplementsList");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("{");
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				printToken(",");
				listElements.next().accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "EnumBodyInternal");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("}");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("AnnotationInnerAnnotation")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "Modifiers");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("@");
			printToken("interface");
			{
				FSTNode v=getChild(nonTerminal, "Id");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("{");
			for (FSTNode v : getChildren(nonTerminal,"AnnotationTypeMemberDeclaration")) {
				v.accept(this);
			}
			printToken("}");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("AnnotationInnerClass")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "Modifiers");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ClassOrInterface");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "Id");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "TypeParameters");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ExtendsList");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ImplementsList");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("{");
			hintIncIndent();
			hintNewLine();
			for (FSTNode v : getChildren(nonTerminal,"ClassOrInterfaceBodyDeclaration")) {
				v.accept(this);
				hintNewLine();
				hintNewLine();
			}
			hintDecIndent();
			hintNewLine();
			printToken("}");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("AnnotationInnerEnum")) {
			printFeatures(nonTerminal,true);
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "EnumConstant").iterator();
			{
				FSTNode v=getChild(nonTerminal, "Modifiers");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("enum");
			{
				FSTNode v=getChild(nonTerminal, "Id");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ImplementsList");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("{");
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				printToken(",");
				listElements.next().accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "EnumBodyInternal");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("}");
			printFeatures(nonTerminal,false);
			return false;
		}
		throw new RuntimeException("Unknown Non Terminal in FST "+nonTerminal);
	}
	protected boolean isSubtype(String type, String expectedType) {
		if (type.equals(expectedType)) return true;
		if (type.equals("ClassDeclaration") && expectedType.equals("TypeDeclaration")) return true;
		if (type.equals("AllocationExpressionInit1") && expectedType.equals("AllocationExpressionInit")) return true;
		if (type.equals("TypeArgument1") && expectedType.equals("TypeArgument")) return true;
		if (type.equals("ReferenceTypeP2") && expectedType.equals("ReferenceTypeP")) return true;
		if (type.equals("AssignmentOperator10") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("Statement10") && expectedType.equals("Statement")) return true;
		if (type.equals("PrimitiveType5") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("ExplicitConstructorInvocation1") && expectedType.equals("ExplicitConstructorInvocation")) return true;
		if (type.equals("StatementExpressionAssignment2") && expectedType.equals("StatementExpressionAssignment")) return true;
		if (type.equals("AssignmentOperator2") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("ArrayDimsAndInits1") && expectedType.equals("ArrayDimsAndInits")) return true;
		if (type.equals("Modifier2") && expectedType.equals("Modifier")) return true;
		if (type.equals("RelationalOp2") && expectedType.equals("RelationalOp")) return true;
		if (type.equals("ForInit1") && expectedType.equals("ForInit")) return true;
		if (type.equals("CastExpression2") && expectedType.equals("CastExpression")) return true;
		if (type.equals("TryStatementEnd2") && expectedType.equals("TryStatementEnd")) return true;
		if (type.equals("Annotation3") && expectedType.equals("Annotation")) return true;
		if (type.equals("UnaryExpression3") && expectedType.equals("UnaryExpression")) return true;
		if (type.equals("Statement3") && expectedType.equals("Statement")) return true;
		if (type.equals("PostfixOp1") && expectedType.equals("PostfixOp")) return true;
		if (type.equals("CastLAOp3") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("MethodDecl") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("AllocationExpressionInit2") && expectedType.equals("AllocationExpressionInit")) return true;
		if (type.equals("ReferenceTypeP1") && expectedType.equals("ReferenceTypeP")) return true;
		if (type.equals("SwitchLabel2") && expectedType.equals("SwitchLabel")) return true;
		if (type.equals("Statement11") && expectedType.equals("Statement")) return true;
		if (type.equals("AssignmentOperator9") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("Literal1") && expectedType.equals("Literal")) return true;
		if (type.equals("StatementExpressionAssignment3") && expectedType.equals("StatementExpressionAssignment")) return true;
		if (type.equals("PrimitiveType6") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("ArrayDimsAndInits2") && expectedType.equals("ArrayDimsAndInits")) return true;
		if (type.equals("Modifier3") && expectedType.equals("Modifier")) return true;
		if (type.equals("Modifier13") && expectedType.equals("Modifier")) return true;
		if (type.equals("InnerClassDecl") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("AssignmentOperator3") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("RelationalOp3") && expectedType.equals("RelationalOp")) return true;
		if (type.equals("PrimarySuffix6") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("CastExpression1") && expectedType.equals("CastExpression")) return true;
		if (type.equals("Statement16") && expectedType.equals("Statement")) return true;
		if (type.equals("TryStatementEnd1") && expectedType.equals("TryStatementEnd")) return true;
		if (type.equals("AllocationExpression2") && expectedType.equals("AllocationExpression")) return true;
		if (type.equals("PostfixOp2") && expectedType.equals("PostfixOp")) return true;
		if (type.equals("Statement2") && expectedType.equals("Statement")) return true;
		if (type.equals("UnaryExpression2") && expectedType.equals("UnaryExpression")) return true;
		if (type.equals("ForInit2") && expectedType.equals("ForInit")) return true;
		if (type.equals("CastLAOp4") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("SwitchLabel1") && expectedType.equals("SwitchLabel")) return true;
		if (type.equals("Modifier4") && expectedType.equals("Modifier")) return true;
		if (type.equals("AnnotationFieldDecl") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("FieldDecl") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("Statement9") && expectedType.equals("Statement")) return true;
		if (type.equals("ShiftOp2") && expectedType.equals("ShiftOp")) return true;
		if (type.equals("PrimitiveType7") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("CastLAOp9") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("AdditiveOp1") && expectedType.equals("AdditiveOp")) return true;
		if (type.equals("CastLookahead2") && expectedType.equals("CastLookahead")) return true;
		if (type.equals("Type2") && expectedType.equals("Type")) return true;
		if (type.equals("EqualityOp1") && expectedType.equals("EqualityOp")) return true;
		if (type.equals("AdditiveOp2") && expectedType.equals("AdditiveOp")) return true;
		if (type.equals("PrimarySuffix5") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("PrimaryPrefix1") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("MemberValue1") && expectedType.equals("MemberValue")) return true;
		if (type.equals("CastLAOp1") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("InitializerDecl") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("MultiplicativeOp3") && expectedType.equals("MultiplicativeOp")) return true;
		if (type.equals("Literal6") && expectedType.equals("Literal")) return true;
		if (type.equals("Statement1") && expectedType.equals("Statement")) return true;
		if (type.equals("AssignmentOperator12") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("MethodDeclarationBody2") && expectedType.equals("MethodDeclarationBody")) return true;
		if (type.equals("UnaryOp2") && expectedType.equals("UnaryOp")) return true;
		if (type.equals("Statement8") && expectedType.equals("Statement")) return true;
		if (type.equals("TypeArgument2") && expectedType.equals("TypeArgument")) return true;
		if (type.equals("Modifier5") && expectedType.equals("Modifier")) return true;
		if (type.equals("ShiftOp1") && expectedType.equals("ShiftOp")) return true;
		if (type.equals("PrimarySuffix3") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("RelationalOp1") && expectedType.equals("RelationalOp")) return true;
		if (type.equals("PrimitiveType8") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("AnnotationInnerClass") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("AssignmentOperator1") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("StatementExpressionAssignment1") && expectedType.equals("StatementExpressionAssignment")) return true;
		if (type.equals("CastLookahead1") && expectedType.equals("CastLookahead")) return true;
		if (type.equals("PrimaryPrefix8") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("EqualityOp2") && expectedType.equals("EqualityOp")) return true;
		if (type.equals("Type1") && expectedType.equals("Type")) return true;
		if (type.equals("PrimaryPrefix2") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("MemberValue2") && expectedType.equals("MemberValue")) return true;
		if (type.equals("PrimarySuffix4") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("UnaryExpression4") && expectedType.equals("UnaryExpression")) return true;
		if (type.equals("MultiplicativeOp2") && expectedType.equals("MultiplicativeOp")) return true;
		if (type.equals("CastLAOp2") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("AssignmentOperator11") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("AssignmentOperator6") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("BlockStatement3") && expectedType.equals("BlockStatement")) return true;
		if (type.equals("CastLAOp7") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("Statement14") && expectedType.equals("Statement")) return true;
		if (type.equals("WildcardBounds1") && expectedType.equals("WildcardBounds")) return true;
		if (type.equals("VariableInitializer1") && expectedType.equals("VariableInitializer")) return true;
		if (type.equals("MultiplicativeOp1") && expectedType.equals("MultiplicativeOp")) return true;
		if (type.equals("StatementExpression1") && expectedType.equals("StatementExpression")) return true;
		if (type.equals("Modifier10") && expectedType.equals("Modifier")) return true;
		if (type.equals("Modifier6") && expectedType.equals("Modifier")) return true;
		if (type.equals("ClassOrInterface2") && expectedType.equals("ClassOrInterface")) return true;
		if (type.equals("UnaryOp1") && expectedType.equals("UnaryOp")) return true;
		if (type.equals("EnumDecl") && expectedType.equals("TypeDeclaration")) return true;
		if (type.equals("EmptyDecl") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("ForStatementInternal2") && expectedType.equals("ForStatementInternal")) return true;
		if (type.equals("PrimarySuffix2") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("Literal4") && expectedType.equals("Literal")) return true;
		if (type.equals("Statement7") && expectedType.equals("Statement")) return true;
		if (type.equals("MemberValue3") && expectedType.equals("MemberValue")) return true;
		if (type.equals("AnnotationInnerAnnotation") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("PrimitiveType1") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("UnaryExpressionNotPlusMinus2") && expectedType.equals("UnaryExpressionNotPlusMinus")) return true;
		if (type.equals("PrimaryPrefix7") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("AssignmentOperator5") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("AnnotationInnerEnum") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("EmptyTypeDecl") && expectedType.equals("TypeDeclaration")) return true;
		if (type.equals("PrimitiveType2") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("CastLAOp8") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("Statement15") && expectedType.equals("Statement")) return true;
		if (type.equals("WildcardBounds2") && expectedType.equals("WildcardBounds")) return true;
		if (type.equals("VariableInitializer2") && expectedType.equals("VariableInitializer")) return true;
		if (type.equals("BlockStatement2") && expectedType.equals("BlockStatement")) return true;
		if (type.equals("ShiftOp3") && expectedType.equals("ShiftOp")) return true;
		if (type.equals("Modifier7") && expectedType.equals("Modifier")) return true;
		if (type.equals("ClassOrInterface1") && expectedType.equals("ClassOrInterface")) return true;
		if (type.equals("UnaryExpressionNotPlusMinus3") && expectedType.equals("UnaryExpressionNotPlusMinus")) return true;
		if (type.equals("ForStatementInternal1") && expectedType.equals("ForStatementInternal")) return true;
		if (type.equals("CastLookahead3") && expectedType.equals("CastLookahead")) return true;
		if (type.equals("Statement6") && expectedType.equals("Statement")) return true;
		if (type.equals("PrimarySuffix1") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("Literal5") && expectedType.equals("Literal")) return true;
		if (type.equals("AssignmentOperator4") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("PrimaryPrefix6") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("StatementExpression3") && expectedType.equals("StatementExpression")) return true;
		if (type.equals("BlockStatement1") && expectedType.equals("BlockStatement")) return true;
		if (type.equals("RelationalOp4") && expectedType.equals("RelationalOp")) return true;
		if (type.equals("CastLAOp5") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("ResultType1") && expectedType.equals("ResultType")) return true;
		if (type.equals("AnnotationTypeDeclaration") && expectedType.equals("TypeDeclaration")) return true;
		if (type.equals("Annotation1") && expectedType.equals("Annotation")) return true;
		if (type.equals("PrimitiveType3") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("Statement12") && expectedType.equals("Statement")) return true;
		if (type.equals("PrimaryPrefix4") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("AssignmentOperator8") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("ConditionalExpression2") && expectedType.equals("ConditionalExpression")) return true;
		if (type.equals("Modifier8") && expectedType.equals("Modifier")) return true;
		if (type.equals("MethodDeclarationBody1") && expectedType.equals("MethodDeclarationBody")) return true;
		if (type.equals("Modifier12") && expectedType.equals("Modifier")) return true;
		if (type.equals("Statement5") && expectedType.equals("Statement")) return true;
		if (type.equals("AnnoationEmptyDecl") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("Literal2") && expectedType.equals("Literal")) return true;
		if (type.equals("AnnotationMethodDecl") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("UnaryExpression1") && expectedType.equals("UnaryExpression")) return true;
		if (type.equals("AllocationExpression1") && expectedType.equals("AllocationExpression")) return true;
		if (type.equals("PrimaryPrefix5") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("Statement13") && expectedType.equals("Statement")) return true;
		if (type.equals("ResultType2") && expectedType.equals("ResultType")) return true;
		if (type.equals("StatementExpression2") && expectedType.equals("StatementExpression")) return true;
		if (type.equals("Annotation2") && expectedType.equals("Annotation")) return true;
		if (type.equals("PrimaryPrefix3") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("CastLAOp6") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("AssignmentOperator7") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("PrimitiveType4") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("ExplicitConstructorInvocation2") && expectedType.equals("ExplicitConstructorInvocation")) return true;
		if (type.equals("Modifier9") && expectedType.equals("Modifier")) return true;
		if (type.equals("ConditionalExpression1") && expectedType.equals("ConditionalExpression")) return true;
		if (type.equals("Modifier11") && expectedType.equals("Modifier")) return true;
		if (type.equals("Modifier1") && expectedType.equals("Modifier")) return true;
		if (type.equals("Statement4") && expectedType.equals("Statement")) return true;
		if (type.equals("ConstructorDecl") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("Literal3") && expectedType.equals("Literal")) return true;
		if (type.equals("BooleanLiteral2") && expectedType.equals("BooleanLiteral")) return true;
		if (type.equals("InnerEnumDecl") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("BooleanLiteral1") && expectedType.equals("BooleanLiteral")) return true;
		if (type.equals("UnaryExpressionNotPlusMinus1") && expectedType.equals("UnaryExpressionNotPlusMinus")) return true;
		return false;
	}
}
