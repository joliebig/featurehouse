package de.ovgu.cide.fstgen.parsers.generated_jml_contract_composition;

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
				FSTNode v=getChild(nonTerminal, "PackageDeclaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			for (FSTNode v : getChildren(nonTerminal,"ImportDeclarationWr")) {
				v.accept(this);
			}
			for (FSTNode v : getChildren(nonTerminal,"TypeDeclaration")) {
				v.accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Modifiers")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "Modifiers2");
				if (v!=null) {
					v.accept(this);
				}
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
		if (nonTerminal.getType().equals("ClassOrInterfaceBodyDeclaration1")) {
			printFeatures(nonTerminal,true);
			printToken("/*@");
			{
				FSTNode v=getChild(nonTerminal, "JMLDeclaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("@*/");
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
		if (nonTerminal.getType().equals("ConstructorDeclarationWithSpec")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "MethodSpecification");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintNewLine();
			{
				FSTNode v=getChild(nonTerminal, "ConstructorDecl");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("FieldDecl")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "ModFieldDeclaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("MethodDeclarationWithSpec")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "MethodSpecification");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintNewLine();
			{
				FSTNode v=getChild(nonTerminal, "MethodDecl");
				if (v!=null) {
					v.accept(this);
				}
			}
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
		if (nonTerminal.getType().equals("JMLDeclaration1")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"Modifier")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "Invariant");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("JMLDeclaration2")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"Modifier")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "HistoryConstraint");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("JMLDeclaration3")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"Modifier")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "RepresentsClause");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("JMLDeclaration4")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"Modifier")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "InitiallyClause");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("JMLDeclaration5")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"Modifier")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "MonitorsForClause");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("JMLDeclaration6")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"Modifier")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "ReadableIfClause");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("JMLDeclaration7")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"Modifier")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "WritableIfClause");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("JMLDeclaration8")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"Modifier")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "AxiomClause");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("JMLDeclaration9")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"Modifier")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "DataGroupClause");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("JMLDeclaration10")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"Modifier")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "MapsIntoClause");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("MethodSpecification")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "AlsoKeyword");
				if (v!=null) {
					printToken("/*@");
					v.accept(this);
					printToken("@*/");
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "Specification");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Specification")) {
			printFeatures(nonTerminal,true);
			printToken("/*@");
			{
				FSTNode v=getChild(nonTerminal, "ContractCompKey");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "SpecCaseSeq");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "RedundantSpec");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("@*/");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("GenericSpecCase1")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "OriginalCaseKeyword");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("GenericSpecCase2")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "SpecVarDecls");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "SpecHeader");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "GenericSpecBody");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("GenericSpecCase3")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "SpecVarDecls");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "GenericSpecBody");
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
		if (type.equals("ClassDeclaration") && expectedType.equals("TypeDeclaration")) return true;
		if (type.equals("TypeArgument1") && expectedType.equals("TypeArgument")) return true;
		if (type.equals("HenceByKeyword1") && expectedType.equals("HenceByKeyword")) return true;
		if (type.equals("RefiningStatement2") && expectedType.equals("RefiningStatement")) return true;
		if (type.equals("SpecArrayRefExpr2") && expectedType.equals("SpecArrayRefExpr")) return true;
		if (type.equals("AssumeKeyword2") && expectedType.equals("AssumeKeyword")) return true;
		if (type.equals("Statement17") && expectedType.equals("Statement")) return true;
		if (type.equals("ExplicitConstructorInvocation1") && expectedType.equals("ExplicitConstructorInvocation")) return true;
		if (type.equals("SimpleSpecBodyClause1") && expectedType.equals("SimpleSpecBodyClause")) return true;
		if (type.equals("StatementExpressionAssignment2") && expectedType.equals("StatementExpressionAssignment")) return true;
		if (type.equals("JMLModifier10") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("PrimarySuffix7") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("DurationClause2") && expectedType.equals("DurationClause")) return true;
		if (type.equals("NonStarsClose1") && expectedType.equals("NonStarsClose")) return true;
		if (type.equals("BreaksKeyword2") && expectedType.equals("BreaksKeyword")) return true;
		if (type.equals("Statement3") && expectedType.equals("Statement")) return true;
		if (type.equals("SpecVarDecls1") && expectedType.equals("SpecVarDecls")) return true;
		if (type.equals("CastLAOp3") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("MapsKeyword1") && expectedType.equals("MapsKeyword")) return true;
		if (type.equals("AccessibleKeyword2") && expectedType.equals("AccessibleKeyword")) return true;
		if (type.equals("SimpleSpecStatementClause15") && expectedType.equals("SimpleSpecStatementClause")) return true;
		if (type.equals("SpecArrayRefExpr3") && expectedType.equals("SpecArrayRefExpr")) return true;
		if (type.equals("SimpleSpecStatementClause14") && expectedType.equals("SimpleSpecStatementClause")) return true;
		if (type.equals("SpecVarDecls2") && expectedType.equals("SpecVarDecls")) return true;
		if (type.equals("MeasuredClause1") && expectedType.equals("MeasuredClause")) return true;
		if (type.equals("StatementExpressionAssignment3") && expectedType.equals("StatementExpressionAssignment")) return true;
		if (type.equals("Modifier13") && expectedType.equals("Modifier")) return true;
		if (type.equals("PrimarySuffix6") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("Statement16") && expectedType.equals("Statement")) return true;
		if (type.equals("NonStarsClose2") && expectedType.equals("NonStarsClose")) return true;
		if (type.equals("AllocationExpression2") && expectedType.equals("AllocationExpression")) return true;
		if (type.equals("Statement2") && expectedType.equals("Statement")) return true;
		if (type.equals("BreaksKeyword1") && expectedType.equals("BreaksKeyword")) return true;
		if (type.equals("CastLAOp4") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("CapturesKeyword1") && expectedType.equals("CapturesKeyword")) return true;
		if (type.equals("JMLAnnotationStatement1") && expectedType.equals("JMLAnnotationStatement")) return true;
		if (type.equals("AnnotationFieldDecl") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("StoreRefList2") && expectedType.equals("StoreRefList")) return true;
		if (type.equals("Statement19") && expectedType.equals("Statement")) return true;
		if (type.equals("Type2") && expectedType.equals("Type")) return true;
		if (type.equals("JMLModifier12") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("PrimarySuffix5") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("PrimaryPrefix1") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("MethodRefStart2") && expectedType.equals("MethodRefStart")) return true;
		if (type.equals("CastLAOp1") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("GroupNamePrefix1") && expectedType.equals("GroupNamePrefix")) return true;
		if (type.equals("Statement1") && expectedType.equals("Statement")) return true;
		if (type.equals("InKeyword2") && expectedType.equals("InKeyword")) return true;
		if (type.equals("CapturesKeyword2") && expectedType.equals("CapturesKeyword")) return true;
		if (type.equals("SpecArrayRefExpr1") && expectedType.equals("SpecArrayRefExpr")) return true;
		if (type.equals("JMLAnnotationStatement2") && expectedType.equals("JMLAnnotationStatement")) return true;
		if (type.equals("HenceByKeyword2") && expectedType.equals("HenceByKeyword")) return true;
		if (type.equals("TypeArgument2") && expectedType.equals("TypeArgument")) return true;
		if (type.equals("PrimarySuffix3") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("StoreRefList1") && expectedType.equals("StoreRefList")) return true;
		if (type.equals("StatementExpressionAssignment1") && expectedType.equals("StatementExpressionAssignment")) return true;
		if (type.equals("Statement18") && expectedType.equals("Statement")) return true;
		if (type.equals("Type1") && expectedType.equals("Type")) return true;
		if (type.equals("PrimaryPrefix2") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("MethodRefStart3") && expectedType.equals("MethodRefStart")) return true;
		if (type.equals("PrimarySuffix4") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("JMLModifier11") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("DurationClause1") && expectedType.equals("DurationClause")) return true;
		if (type.equals("AccessibleKeyword1") && expectedType.equals("AccessibleKeyword")) return true;
		if (type.equals("CastLAOp2") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("InKeyword1") && expectedType.equals("InKeyword")) return true;
		if (type.equals("JMLAnnotationStatement7") && expectedType.equals("JMLAnnotationStatement")) return true;
		if (type.equals("JMLDeclaration9") && expectedType.equals("JMLDeclaration")) return true;
		if (type.equals("MultiplicativeOp1") && expectedType.equals("MultiplicativeOp")) return true;
		if (type.equals("StatementExpression1") && expectedType.equals("StatementExpression")) return true;
		if (type.equals("Modifier10") && expectedType.equals("Modifier")) return true;
		if (type.equals("JMLAnnotationStatement3") && expectedType.equals("JMLAnnotationStatement")) return true;
		if (type.equals("SimpleSpecStatementClause11") && expectedType.equals("SimpleSpecStatementClause")) return true;
		if (type.equals("ClassOrInterface2") && expectedType.equals("ClassOrInterface")) return true;
		if (type.equals("GenericSpecStatementBody2") && expectedType.equals("GenericSpecStatementBody")) return true;
		if (type.equals("Statement7") && expectedType.equals("Statement")) return true;
		if (type.equals("AnnotationInnerAnnotation") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("JMLModifier14") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("SimpleSpecBodyClause5") && expectedType.equals("SimpleSpecBodyClause")) return true;
		if (type.equals("JMLDeclaration8") && expectedType.equals("JMLDeclaration")) return true;
		if (type.equals("ConstrainedList2") && expectedType.equals("ConstrainedList")) return true;
		if (type.equals("JMLAnnotationStatement4") && expectedType.equals("JMLAnnotationStatement")) return true;
		if (type.equals("ClassOrInterface1") && expectedType.equals("ClassOrInterface")) return true;
		if (type.equals("SimpleSpecStatementClause10") && expectedType.equals("SimpleSpecStatementClause")) return true;
		if (type.equals("Statement6") && expectedType.equals("Statement")) return true;
		if (type.equals("JMLModifier13") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("SimpleSpecBodyClause4") && expectedType.equals("SimpleSpecBodyClause")) return true;
		if (type.equals("ConstraintKeyword1") && expectedType.equals("ConstraintKeyword")) return true;
		if (type.equals("StatementExpression3") && expectedType.equals("StatementExpression")) return true;
		if (type.equals("JMLAnnotationStatement5") && expectedType.equals("JMLAnnotationStatement")) return true;
		if (type.equals("ConstrainedList1") && expectedType.equals("ConstrainedList")) return true;
		if (type.equals("MethodRef2") && expectedType.equals("MethodRef")) return true;
		if (type.equals("AnnotationTypeDeclaration") && expectedType.equals("TypeDeclaration")) return true;
		if (type.equals("OldExpression1") && expectedType.equals("OldExpression")) return true;
		if (type.equals("JMLModifier17") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("ImpliesExpr1") && expectedType.equals("ImpliesExpr")) return true;
		if (type.equals("SimpleSpecStatementClause13") && expectedType.equals("SimpleSpecStatementClause")) return true;
		if (type.equals("Modifier12") && expectedType.equals("Modifier")) return true;
		if (type.equals("JmlPrimary20") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("Statement5") && expectedType.equals("Statement")) return true;
		if (type.equals("AllocationExpression1") && expectedType.equals("AllocationExpression")) return true;
		if (type.equals("StoreRefKeyword3") && expectedType.equals("StoreRefKeyword")) return true;
		if (type.equals("SimpleSpecBodyClause3") && expectedType.equals("SimpleSpecBodyClause")) return true;
		if (type.equals("JMLAnnotationStatement6") && expectedType.equals("JMLAnnotationStatement")) return true;
		if (type.equals("ConstraintKeyword2") && expectedType.equals("ConstraintKeyword")) return true;
		if (type.equals("StatementExpression2") && expectedType.equals("StatementExpression")) return true;
		if (type.equals("JMLModifier16") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("MethodRef1") && expectedType.equals("MethodRef")) return true;
		if (type.equals("ExplicitConstructorInvocation2") && expectedType.equals("ExplicitConstructorInvocation")) return true;
		if (type.equals("SimpleSpecBodyClause2") && expectedType.equals("SimpleSpecBodyClause")) return true;
		if (type.equals("ImpliesExpr2") && expectedType.equals("ImpliesExpr")) return true;
		if (type.equals("JmlPrimary21") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("Modifier11") && expectedType.equals("Modifier")) return true;
		if (type.equals("SimpleSpecStatementClause12") && expectedType.equals("SimpleSpecStatementClause")) return true;
		if (type.equals("Modifier1") && expectedType.equals("Modifier")) return true;
		if (type.equals("Statement4") && expectedType.equals("Statement")) return true;
		if (type.equals("GenericSpecStatementBody1") && expectedType.equals("GenericSpecStatementBody")) return true;
		if (type.equals("JMLModifier15") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("StoreRefKeyword2") && expectedType.equals("StoreRefKeyword")) return true;
		if (type.equals("AllocationExpressionInit1") && expectedType.equals("AllocationExpressionInit")) return true;
		if (type.equals("OriginalCaseKeyword1") && expectedType.equals("OriginalCaseKeyword")) return true;
		if (type.equals("ContractCompKeyList3") && expectedType.equals("ContractCompKeyList")) return true;
		if (type.equals("ReferenceTypeP2") && expectedType.equals("ReferenceTypeP")) return true;
		if (type.equals("JMLDeclaration4") && expectedType.equals("JMLDeclaration")) return true;
		if (type.equals("AssignmentOperator10") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("PredOrNot1") && expectedType.equals("PredOrNot")) return true;
		if (type.equals("StoreRef2") && expectedType.equals("StoreRef")) return true;
		if (type.equals("GenericSpecStatementCase2") && expectedType.equals("GenericSpecStatementCase")) return true;
		if (type.equals("Modifier2") && expectedType.equals("Modifier")) return true;
		if (type.equals("ForInit1") && expectedType.equals("ForInit")) return true;
		if (type.equals("GenericSpecStatementCase1") && expectedType.equals("GenericSpecStatementCase")) return true;
		if (type.equals("AssignableKeyword2") && expectedType.equals("AssignableKeyword")) return true;
		if (type.equals("AllocationExpressionInit2") && expectedType.equals("AllocationExpressionInit")) return true;
		if (type.equals("OriginalCaseKeyword2") && expectedType.equals("OriginalCaseKeyword")) return true;
		if (type.equals("WorkingSpaceKeyword2") && expectedType.equals("WorkingSpaceKeyword")) return true;
		if (type.equals("ContractCompKeyList2") && expectedType.equals("ContractCompKeyList")) return true;
		if (type.equals("ReferenceTypeP1") && expectedType.equals("ReferenceTypeP")) return true;
		if (type.equals("JMLDeclaration3") && expectedType.equals("JMLDeclaration")) return true;
		if (type.equals("PredOrNot2") && expectedType.equals("PredOrNot")) return true;
		if (type.equals("StoreRef1") && expectedType.equals("StoreRef")) return true;
		if (type.equals("AssignmentOperator9") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("Modifier3") && expectedType.equals("Modifier")) return true;
		if (type.equals("SimpleSpecBodyClause12") && expectedType.equals("SimpleSpecBodyClause")) return true;
		if (type.equals("GenericSpecCase3") && expectedType.equals("GenericSpecCase")) return true;
		if (type.equals("AssignableKeyword3") && expectedType.equals("AssignableKeyword")) return true;
		if (type.equals("OriginalClauseKeyword1") && expectedType.equals("OriginalClauseKeyword")) return true;
		if (type.equals("RepresentsKeyword2") && expectedType.equals("RepresentsKeyword")) return true;
		if (type.equals("ForInit2") && expectedType.equals("ForInit")) return true;
		if (type.equals("Modifier4") && expectedType.equals("Modifier")) return true;
		if (type.equals("ContractCompKeyList1") && expectedType.equals("ContractCompKeyList")) return true;
		if (type.equals("Statement9") && expectedType.equals("Statement")) return true;
		if (type.equals("SimpleSpecStatementClause8") && expectedType.equals("SimpleSpecStatementClause")) return true;
		if (type.equals("EquivalenceOp2") && expectedType.equals("EquivalenceOp")) return true;
		if (type.equals("GenericSpecCase2") && expectedType.equals("GenericSpecCase")) return true;
		if (type.equals("EqualityOp1") && expectedType.equals("EqualityOp")) return true;
		if (type.equals("AssignableKeyword4") && expectedType.equals("AssignableKeyword")) return true;
		if (type.equals("SimpleSpecBodyClause11") && expectedType.equals("SimpleSpecBodyClause")) return true;
		if (type.equals("MemberValue1") && expectedType.equals("MemberValue")) return true;
		if (type.equals("MultiplicativeOp3") && expectedType.equals("MultiplicativeOp")) return true;
		if (type.equals("RepresentsKeyword1") && expectedType.equals("RepresentsKeyword")) return true;
		if (type.equals("WorkingSpaceClause2") && expectedType.equals("WorkingSpaceClause")) return true;
		if (type.equals("MethodName2") && expectedType.equals("MethodName")) return true;
		if (type.equals("AssignmentOperator12") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("JMLDeclaration7") && expectedType.equals("JMLDeclaration")) return true;
		if (type.equals("MethodDeclarationBody2") && expectedType.equals("MethodDeclarationBody")) return true;
		if (type.equals("JMLDeclaration5") && expectedType.equals("JMLDeclaration")) return true;
		if (type.equals("DivergesKeyword1") && expectedType.equals("DivergesKeyword")) return true;
		if (type.equals("Statement8") && expectedType.equals("Statement")) return true;
		if (type.equals("SimpleSpecStatementClause9") && expectedType.equals("SimpleSpecStatementClause")) return true;
		if (type.equals("Modifier5") && expectedType.equals("Modifier")) return true;
		if (type.equals("AnnotationInnerClass") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("BlockStatement4") && expectedType.equals("BlockStatement")) return true;
		if (type.equals("GenericSpecCase1") && expectedType.equals("GenericSpecCase")) return true;
		if (type.equals("EqualityOp2") && expectedType.equals("EqualityOp")) return true;
		if (type.equals("AssignableKeyword5") && expectedType.equals("AssignableKeyword")) return true;
		if (type.equals("MemberValue2") && expectedType.equals("MemberValue")) return true;
		if (type.equals("SimpleSpecBodyClause10") && expectedType.equals("SimpleSpecBodyClause")) return true;
		if (type.equals("RequiresKeyword4") && expectedType.equals("RequiresKeyword")) return true;
		if (type.equals("MultiplicativeOp2") && expectedType.equals("MultiplicativeOp")) return true;
		if (type.equals("MethodName1") && expectedType.equals("MethodName")) return true;
		if (type.equals("AssignmentOperator11") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("JMLDeclaration6") && expectedType.equals("JMLDeclaration")) return true;
		if (type.equals("AssignmentOperator6") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("BlockStatement3") && expectedType.equals("BlockStatement")) return true;
		if (type.equals("CastLAOp7") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("EnsuresKeyword4") && expectedType.equals("EnsuresKeyword")) return true;
		if (type.equals("WildcardBounds1") && expectedType.equals("WildcardBounds")) return true;
		if (type.equals("JmlPrimary9") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("SimpleSpecStatementClause6") && expectedType.equals("SimpleSpecStatementClause")) return true;
		if (type.equals("Modifier6") && expectedType.equals("Modifier")) return true;
		if (type.equals("EnumDecl") && expectedType.equals("TypeDeclaration")) return true;
		if (type.equals("PrimarySuffix2") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("MemberValue3") && expectedType.equals("MemberValue")) return true;
		if (type.equals("AssignmentOperator5") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("EmptyTypeDecl") && expectedType.equals("TypeDeclaration")) return true;
		if (type.equals("EnsuresKeyword3") && expectedType.equals("EnsuresKeyword")) return true;
		if (type.equals("EquivalenceOp1") && expectedType.equals("EquivalenceOp")) return true;
		if (type.equals("CastLAOp8") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("WildcardBounds2") && expectedType.equals("WildcardBounds")) return true;
		if (type.equals("BlockStatement2") && expectedType.equals("BlockStatement")) return true;
		if (type.equals("Modifier7") && expectedType.equals("Modifier")) return true;
		if (type.equals("SimpleSpecStatementClause7") && expectedType.equals("SimpleSpecStatementClause")) return true;
		if (type.equals("WorkingSpaceClause1") && expectedType.equals("WorkingSpaceClause")) return true;
		if (type.equals("PrimarySuffix1") && expectedType.equals("PrimarySuffix")) return true;
		if (type.equals("AssignmentOperator4") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("MemberValue4") && expectedType.equals("MemberValue")) return true;
		if (type.equals("BlockStatement1") && expectedType.equals("BlockStatement")) return true;
		if (type.equals("JmlPrimary7") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("CastLAOp5") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("ContractCompKeyList5") && expectedType.equals("ContractCompKeyList")) return true;
		if (type.equals("ResultType1") && expectedType.equals("ResultType")) return true;
		if (type.equals("EnsuresKeyword2") && expectedType.equals("EnsuresKeyword")) return true;
		if (type.equals("AssignmentOperator8") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("Modifier8") && expectedType.equals("Modifier")) return true;
		if (type.equals("JMLDeclaration2") && expectedType.equals("JMLDeclaration")) return true;
		if (type.equals("SimpleSpecStatementClause4") && expectedType.equals("SimpleSpecStatementClause")) return true;
		if (type.equals("AnnoationEmptyDecl") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("MeasuredClause2") && expectedType.equals("MeasuredClause")) return true;
		if (type.equals("AnnotationMethodDecl") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("OriginalClauseKeyword2") && expectedType.equals("OriginalClauseKeyword")) return true;
		if (type.equals("DurationKeyword1") && expectedType.equals("DurationKeyword")) return true;
		if (type.equals("InvariantKeyword2") && expectedType.equals("InvariantKeyword")) return true;
		if (type.equals("JmlPrimary8") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("ResultType2") && expectedType.equals("ResultType")) return true;
		if (type.equals("CastLAOp6") && expectedType.equals("CastLAOp")) return true;
		if (type.equals("AssignmentOperator7") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("EnsuresKeyword1") && expectedType.equals("EnsuresKeyword")) return true;
		if (type.equals("SimpleSpecStatementClause5") && expectedType.equals("SimpleSpecStatementClause")) return true;
		if (type.equals("Modifier9") && expectedType.equals("Modifier")) return true;
		if (type.equals("ContractCompKeyList4") && expectedType.equals("ContractCompKeyList")) return true;
		if (type.equals("JMLDeclaration1") && expectedType.equals("JMLDeclaration")) return true;
		if (type.equals("AssignableKeyword1") && expectedType.equals("AssignableKeyword")) return true;
		if (type.equals("BooleanLiteral1") && expectedType.equals("BooleanLiteral")) return true;
		if (type.equals("InvariantKeyword1") && expectedType.equals("InvariantKeyword")) return true;
		if (type.equals("DurationKeyword2") && expectedType.equals("DurationKeyword")) return true;
		if (type.equals("HeavyWeightSpecCase1") && expectedType.equals("HeavyWeightSpecCase")) return true;
		if (type.equals("CallableMethodsList2") && expectedType.equals("CallableMethodsList")) return true;
		if (type.equals("PrimitiveType5") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("SpecStatement3") && expectedType.equals("SpecStatement")) return true;
		if (type.equals("AssignmentOperator2") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("RelationalOp2") && expectedType.equals("RelationalOp")) return true;
		if (type.equals("Privacy1") && expectedType.equals("Privacy")) return true;
		if (type.equals("TryStatementEnd2") && expectedType.equals("TryStatementEnd")) return true;
		if (type.equals("SignalsKeyword4") && expectedType.equals("SignalsKeyword")) return true;
		if (type.equals("StoreRefNameSuffix4") && expectedType.equals("StoreRefNameSuffix")) return true;
		if (type.equals("JmlPrimary4") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("SignalsOnlyClause2") && expectedType.equals("SignalsOnlyClause")) return true;
		if (type.equals("AssumeKeyword1") && expectedType.equals("AssumeKeyword")) return true;
		if (type.equals("HeavyWeightSpecCase2") && expectedType.equals("HeavyWeightSpecCase")) return true;
		if (type.equals("StoreRefNameSuffix2") && expectedType.equals("StoreRefNameSuffix")) return true;
		if (type.equals("SpecStatement4") && expectedType.equals("SpecStatement")) return true;
		if (type.equals("Literal1") && expectedType.equals("Literal")) return true;
		if (type.equals("PrimitiveType6") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("TypeSpec1") && expectedType.equals("TypeSpec")) return true;
		if (type.equals("AssignmentOperator3") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("JmlPrimary18") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("RelationalOp3") && expectedType.equals("RelationalOp")) return true;
		if (type.equals("Privacy2") && expectedType.equals("Privacy")) return true;
		if (type.equals("JmlPrimary19") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("TryStatementEnd1") && expectedType.equals("TryStatementEnd")) return true;
		if (type.equals("MethodRefRest2") && expectedType.equals("MethodRefRest")) return true;
		if (type.equals("JmlPrimary3") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("StoreRefNameSuffix3") && expectedType.equals("StoreRefNameSuffix")) return true;
		if (type.equals("StoreRefNameSuffix1") && expectedType.equals("StoreRefNameSuffix")) return true;
		if (type.equals("TypeSpec2") && expectedType.equals("TypeSpec")) return true;
		if (type.equals("FieldDecl") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("PrimitiveType7") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("SpecStatement1") && expectedType.equals("SpecStatement")) return true;
		if (type.equals("SignalsKeyword2") && expectedType.equals("SignalsKeyword")) return true;
		if (type.equals("JMLModifier9") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("JmlPrimary6") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("BehaviorKeyword2") && expectedType.equals("BehaviorKeyword")) return true;
		if (type.equals("Privacy3") && expectedType.equals("Privacy")) return true;
		if (type.equals("WhenKeyword2") && expectedType.equals("WhenKeyword")) return true;
		if (type.equals("SimpleSpecStatementClause3") && expectedType.equals("SimpleSpecStatementClause")) return true;
		if (type.equals("CallableMethodsList1") && expectedType.equals("CallableMethodsList")) return true;
		if (type.equals("SimpleSpecStatementClause1") && expectedType.equals("SimpleSpecStatementClause")) return true;
		if (type.equals("RelationalOp1") && expectedType.equals("RelationalOp")) return true;
		if (type.equals("PrimitiveType8") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("AssignmentOperator1") && expectedType.equals("AssignmentOperator")) return true;
		if (type.equals("SpecStatement2") && expectedType.equals("SpecStatement")) return true;
		if (type.equals("BehaviorKeyword1") && expectedType.equals("BehaviorKeyword")) return true;
		if (type.equals("SignalsKeyword3") && expectedType.equals("SignalsKeyword")) return true;
		if (type.equals("WhenKeyword1") && expectedType.equals("WhenKeyword")) return true;
		if (type.equals("SimpleSpecStatementClause2") && expectedType.equals("SimpleSpecStatementClause")) return true;
		if (type.equals("JmlPrimary5") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("AbruptBehaviorKeyword2") && expectedType.equals("AbruptBehaviorKeyword")) return true;
		if (type.equals("VariableInitializer1") && expectedType.equals("VariableInitializer")) return true;
		if (type.equals("JMLModifier7") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("MemberFieldRef1") && expectedType.equals("MemberFieldRef")) return true;
		if (type.equals("RequiresKeyword2") && expectedType.equals("RequiresKeyword")) return true;
		if (type.equals("ForStatementInternal2") && expectedType.equals("ForStatementInternal")) return true;
		if (type.equals("StoreRefName3") && expectedType.equals("StoreRefName")) return true;
		if (type.equals("UnaryExpressionNotPlusMinus2") && expectedType.equals("UnaryExpressionNotPlusMinus")) return true;
		if (type.equals("PrimitiveType1") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("ParamModifier1") && expectedType.equals("ParamModifier")) return true;
		if (type.equals("PrimitiveType2") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("SpecCase3") && expectedType.equals("SpecCase")) return true;
		if (type.equals("SignalsKeyword1") && expectedType.equals("SignalsKeyword")) return true;
		if (type.equals("VariableInitializer2") && expectedType.equals("VariableInitializer")) return true;
		if (type.equals("AssignableKeyword6") && expectedType.equals("AssignableKeyword")) return true;
		if (type.equals("MemberFieldRef2") && expectedType.equals("MemberFieldRef")) return true;
		if (type.equals("RequiresKeyword3") && expectedType.equals("RequiresKeyword")) return true;
		if (type.equals("JMLModifier8") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("UnaryExpressionNotPlusMinus3") && expectedType.equals("UnaryExpressionNotPlusMinus")) return true;
		if (type.equals("ForStatementInternal1") && expectedType.equals("ForStatementInternal")) return true;
		if (type.equals("CastLookahead3") && expectedType.equals("CastLookahead")) return true;
		if (type.equals("StoreRefName2") && expectedType.equals("StoreRefName")) return true;
		if (type.equals("DivergesKeyword2") && expectedType.equals("DivergesKeyword")) return true;
		if (type.equals("ParamModifier2") && expectedType.equals("ParamModifier")) return true;
		if (type.equals("RelationalOp4") && expectedType.equals("RelationalOp")) return true;
		if (type.equals("HeavyWeightSpecCase3") && expectedType.equals("HeavyWeightSpecCase")) return true;
		if (type.equals("Annotation1") && expectedType.equals("Annotation")) return true;
		if (type.equals("PrimitiveType3") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("SpecCase2") && expectedType.equals("SpecCase")) return true;
		if (type.equals("RepresentsClause1") && expectedType.equals("RepresentsClause")) return true;
		if (type.equals("ConditionalExpression2") && expectedType.equals("ConditionalExpression")) return true;
		if (type.equals("JMLModifier5") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("StoreRefName1") && expectedType.equals("StoreRefName")) return true;
		if (type.equals("MethodDeclarationBody1") && expectedType.equals("MethodDeclarationBody")) return true;
		if (type.equals("JmlPrimary2") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("ClassOrInterfaceBodyDeclaration1") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("ParamModifier3") && expectedType.equals("ParamModifier")) return true;
		if (type.equals("ModelProgStatement3") && expectedType.equals("ModelProgStatement")) return true;
		if (type.equals("ReturnsKeyword1") && expectedType.equals("ReturnsKeyword")) return true;
		if (type.equals("AbruptBehaviorKeyword1") && expectedType.equals("AbruptBehaviorKeyword")) return true;
		if (type.equals("Annotation2") && expectedType.equals("Annotation")) return true;
		if (type.equals("PrimitiveType4") && expectedType.equals("PrimitiveType")) return true;
		if (type.equals("SpecCase1") && expectedType.equals("SpecCase")) return true;
		if (type.equals("RequiresKeyword1") && expectedType.equals("RequiresKeyword")) return true;
		if (type.equals("MeasuredByKeyword1") && expectedType.equals("MeasuredByKeyword")) return true;
		if (type.equals("JMLModifier6") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("ConditionalExpression1") && expectedType.equals("ConditionalExpression")) return true;
		if (type.equals("JmlPrimary1") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("SignalsOnlyClause1") && expectedType.equals("SignalsOnlyClause")) return true;
		if (type.equals("ForStatementInternal3") && expectedType.equals("ForStatementInternal")) return true;
		if (type.equals("BooleanLiteral2") && expectedType.equals("BooleanLiteral")) return true;
		if (type.equals("MethodOrConstructorKeyword2") && expectedType.equals("MethodOrConstructorKeyword")) return true;
		if (type.equals("InnerEnumDecl") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("UnaryExpressionNotPlusMinus1") && expectedType.equals("UnaryExpressionNotPlusMinus")) return true;
		if (type.equals("WorkingSpaceKeyword1") && expectedType.equals("WorkingSpaceKeyword")) return true;
		if (type.equals("ModelProgStatement4") && expectedType.equals("ModelProgStatement")) return true;
		if (type.equals("SignalsOnlyKeyword1") && expectedType.equals("SignalsOnlyKeyword")) return true;
		if (type.equals("ModelProgStatement1") && expectedType.equals("ModelProgStatement")) return true;
		if (type.equals("MethodOrConstructorKeyword1") && expectedType.equals("MethodOrConstructorKeyword")) return true;
		if (type.equals("Statement10") && expectedType.equals("Statement")) return true;
		if (type.equals("SimpleSpecBodyClause6") && expectedType.equals("SimpleSpecBodyClause")) return true;
		if (type.equals("ArrayDimsAndInits1") && expectedType.equals("ArrayDimsAndInits")) return true;
		if (type.equals("MapsMemberRefExpr2") && expectedType.equals("MapsMemberRefExpr")) return true;
		if (type.equals("CallableKeyword1") && expectedType.equals("CallableKeyword")) return true;
		if (type.equals("ReturnsKeyword2") && expectedType.equals("ReturnsKeyword")) return true;
		if (type.equals("RepresentsClause3") && expectedType.equals("RepresentsClause")) return true;
		if (type.equals("CallableKeyword2") && expectedType.equals("CallableKeyword")) return true;
		if (type.equals("CastExpression2") && expectedType.equals("CastExpression")) return true;
		if (type.equals("JMLModifier3") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("MeasuredByKeyword2") && expectedType.equals("MeasuredByKeyword")) return true;
		if (type.equals("JmlPrimary25") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("JmlPrimary11") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("Annotation3") && expectedType.equals("Annotation")) return true;
		if (type.equals("Quantifier4") && expectedType.equals("Quantifier")) return true;
		if (type.equals("UnaryExpression3") && expectedType.equals("UnaryExpression")) return true;
		if (type.equals("PostfixOp1") && expectedType.equals("PostfixOp")) return true;
		if (type.equals("GenericSpecBody2") && expectedType.equals("GenericSpecBody")) return true;
		if (type.equals("SpecInitializer2") && expectedType.equals("SpecInitializer")) return true;
		if (type.equals("MapsKeyword2") && expectedType.equals("MapsKeyword")) return true;
		if (type.equals("OldExpression2") && expectedType.equals("OldExpression")) return true;
		if (type.equals("JMLModifier18") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("StoreRefKeyword1") && expectedType.equals("StoreRefKeyword")) return true;
		if (type.equals("SwitchLabel2") && expectedType.equals("SwitchLabel")) return true;
		if (type.equals("ModelProgStatement2") && expectedType.equals("ModelProgStatement")) return true;
		if (type.equals("SimpleSpecBodyClause7") && expectedType.equals("SimpleSpecBodyClause")) return true;
		if (type.equals("Statement11") && expectedType.equals("Statement")) return true;
		if (type.equals("ArrayDimsAndInits2") && expectedType.equals("ArrayDimsAndInits")) return true;
		if (type.equals("InnerClassDecl") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("JMLModifier4") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("CastExpression1") && expectedType.equals("CastExpression")) return true;
		if (type.equals("JmlPrimary24") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("RepresentsClause2") && expectedType.equals("RepresentsClause")) return true;
		if (type.equals("JmlPrimary10") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("ExceptionalBehaviorKeyword2") && expectedType.equals("ExceptionalBehaviorKeyword")) return true;
		if (type.equals("ContinuesKeyword1") && expectedType.equals("ContinuesKeyword")) return true;
		if (type.equals("PostfixOp2") && expectedType.equals("PostfixOp")) return true;
		if (type.equals("GenericSpecBody1") && expectedType.equals("GenericSpecBody")) return true;
		if (type.equals("Quantifier3") && expectedType.equals("Quantifier")) return true;
		if (type.equals("UnaryExpression2") && expectedType.equals("UnaryExpression")) return true;
		if (type.equals("SwitchLabel1") && expectedType.equals("SwitchLabel")) return true;
		if (type.equals("ShiftOp2") && expectedType.equals("ShiftOp")) return true;
		if (type.equals("ExceptionalBehaviorKeyword1") && expectedType.equals("ExceptionalBehaviorKeyword")) return true;
		if (type.equals("CastLookahead2") && expectedType.equals("CastLookahead")) return true;
		if (type.equals("AdditiveOp1") && expectedType.equals("AdditiveOp")) return true;
		if (type.equals("ContinuesKeyword2") && expectedType.equals("ContinuesKeyword")) return true;
		if (type.equals("SimpleSpecBodyClause8") && expectedType.equals("SimpleSpecBodyClause")) return true;
		if (type.equals("EnumConstant1") && expectedType.equals("EnumConstant")) return true;
		if (type.equals("ImportDeclarationWr2") && expectedType.equals("ImportDeclarationWr")) return true;
		if (type.equals("AdditiveOp2") && expectedType.equals("AdditiveOp")) return true;
		if (type.equals("JmlPrimary23") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("Quantifier2") && expectedType.equals("Quantifier")) return true;
		if (type.equals("JMLModifier1") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("InitializerDecl") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("Literal6") && expectedType.equals("Literal")) return true;
		if (type.equals("NormalBehaviorKeyword1") && expectedType.equals("NormalBehaviorKeyword")) return true;
		if (type.equals("JmlPrimary13") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("UnaryOp2") && expectedType.equals("UnaryOp")) return true;
		if (type.equals("SignalsOnlyKeyword2") && expectedType.equals("SignalsOnlyKeyword")) return true;
		if (type.equals("ShiftOp1") && expectedType.equals("ShiftOp")) return true;
		if (type.equals("MapsMemberRefExpr1") && expectedType.equals("MapsMemberRefExpr")) return true;
		if (type.equals("ConstructorDeclarationWithSpec") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("SimpleSpecBodyClause9") && expectedType.equals("SimpleSpecBodyClause")) return true;
		if (type.equals("CastLookahead1") && expectedType.equals("CastLookahead")) return true;
		if (type.equals("Quantifier1") && expectedType.equals("Quantifier")) return true;
		if (type.equals("PrimaryPrefix8") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("JmlPrimary22") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("ImportDeclarationWr1") && expectedType.equals("ImportDeclarationWr")) return true;
		if (type.equals("JMLModifier2") && expectedType.equals("JMLModifier")) return true;
		if (type.equals("UnaryExpression4") && expectedType.equals("UnaryExpression")) return true;
		if (type.equals("MethodDeclarationWithSpec") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("EnumConstant2") && expectedType.equals("EnumConstant")) return true;
		if (type.equals("JmlPrimary12") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("NormalBehaviorKeyword2") && expectedType.equals("NormalBehaviorKeyword")) return true;
		if (type.equals("Statement14") && expectedType.equals("Statement")) return true;
		if (type.equals("MethodRefStart1") && expectedType.equals("MethodRefStart")) return true;
		if (type.equals("UnaryOp1") && expectedType.equals("UnaryOp")) return true;
		if (type.equals("EmptyDecl") && expectedType.equals("ClassOrInterfaceBodyDeclaration")) return true;
		if (type.equals("JmlPrimary15") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("Literal4") && expectedType.equals("Literal")) return true;
		if (type.equals("JMLDeclaration10") && expectedType.equals("JMLDeclaration")) return true;
		if (type.equals("PrimaryPrefix7") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("AnnotationInnerEnum") && expectedType.equals("AnnotationTypeMemberDeclaration")) return true;
		if (type.equals("Statement15") && expectedType.equals("Statement")) return true;
		if (type.equals("ShiftOp3") && expectedType.equals("ShiftOp")) return true;
		if (type.equals("JmlPrimary14") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("Literal5") && expectedType.equals("Literal")) return true;
		if (type.equals("Quantifier7") && expectedType.equals("Quantifier")) return true;
		if (type.equals("PrimaryPrefix6") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("Statement12") && expectedType.equals("Statement")) return true;
		if (type.equals("PrimaryPrefix4") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("MethodRefRest1") && expectedType.equals("MethodRefRest")) return true;
		if (type.equals("GroupNamePrefix2") && expectedType.equals("GroupNamePrefix")) return true;
		if (type.equals("JmlPrimary17") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("Literal2") && expectedType.equals("Literal")) return true;
		if (type.equals("Quantifier6") && expectedType.equals("Quantifier")) return true;
		if (type.equals("UnaryExpression1") && expectedType.equals("UnaryExpression")) return true;
		if (type.equals("PrimaryPrefix5") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("RefiningStatement1") && expectedType.equals("RefiningStatement")) return true;
		if (type.equals("BoundVarModifiers1") && expectedType.equals("BoundVarModifiers")) return true;
		if (type.equals("Statement13") && expectedType.equals("Statement")) return true;
		if (type.equals("PrimaryPrefix3") && expectedType.equals("PrimaryPrefix")) return true;
		if (type.equals("SpecInitializer1") && expectedType.equals("SpecInitializer")) return true;
		if (type.equals("JmlPrimary16") && expectedType.equals("JmlPrimary")) return true;
		if (type.equals("Quantifier5") && expectedType.equals("Quantifier")) return true;
		if (type.equals("Literal3") && expectedType.equals("Literal")) return true;
		if (type.equals("BoundVarModifiers2") && expectedType.equals("BoundVarModifiers")) return true;
		if (type.equals("JmlPrimary26") && expectedType.equals("JmlPrimary")) return true;
		return false;
	}
}
