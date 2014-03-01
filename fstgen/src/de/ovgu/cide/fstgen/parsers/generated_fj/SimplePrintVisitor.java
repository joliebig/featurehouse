package de.ovgu.cide.fstgen.parsers.generated_fj;

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
		if (nonTerminal.getType().equals("TypeDeclaration")) {
			printFeatures(nonTerminal,true);
			printToken("class");
			{
				FSTNode v=getChild(nonTerminal, "Name");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("extends");
			{
				FSTNode v=getChild(nonTerminal, "ExtendedType");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("{");
			for (FSTNode v : getChildren(nonTerminal,"VarDeclaration")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "ClassConstructor");
				if (v!=null) {
					v.accept(this);
				}
			}
			for (FSTNode v : getChildren(nonTerminal,"MethodDeclaration")) {
				v.accept(this);
			}
			printToken("}");
			printFeatures(nonTerminal,false);
			return false;
		}
		throw new RuntimeException("Unknown Non Terminal in FST "+nonTerminal);
	}
	protected boolean isSubtype(String type, String expectedType) {
		if (type.equals(expectedType)) return true;
		if (type.equals("BinaryOperator1") && expectedType.equals("BinaryOperator")) return true;
		if (type.equals("InvokeTarget4") && expectedType.equals("InvokeTarget")) return true;
		if (type.equals("BinaryOperator2") && expectedType.equals("BinaryOperator")) return true;
		if (type.equals("PrimaryExpression3") && expectedType.equals("PrimaryExpression")) return true;
		if (type.equals("PrimaryExpression2") && expectedType.equals("PrimaryExpression")) return true;
		if (type.equals("PlusOrMinus2") && expectedType.equals("PlusOrMinus")) return true;
		if (type.equals("PrimaryExpression4") && expectedType.equals("PrimaryExpression")) return true;
		if (type.equals("PrimaryExpression1") && expectedType.equals("PrimaryExpression")) return true;
		if (type.equals("PrimaryExpression6") && expectedType.equals("PrimaryExpression")) return true;
		if (type.equals("PlusOrMinus1") && expectedType.equals("PlusOrMinus")) return true;
		if (type.equals("PrimaryExpression5") && expectedType.equals("PrimaryExpression")) return true;
		if (type.equals("ExtendedType2") && expectedType.equals("ExtendedType")) return true;
		if (type.equals("Type2") && expectedType.equals("Type")) return true;
		if (type.equals("Type3") && expectedType.equals("Type")) return true;
		if (type.equals("TimesOrDivide1") && expectedType.equals("TimesOrDivide")) return true;
		if (type.equals("Type1") && expectedType.equals("Type")) return true;
		if (type.equals("BinaryOperator3") && expectedType.equals("BinaryOperator")) return true;
		if (type.equals("ExtendedType1") && expectedType.equals("ExtendedType")) return true;
		if (type.equals("BinaryOperator4") && expectedType.equals("BinaryOperator")) return true;
		if (type.equals("PrimaryExpression7") && expectedType.equals("PrimaryExpression")) return true;
		if (type.equals("TimesOrDivide2") && expectedType.equals("TimesOrDivide")) return true;
		if (type.equals("InvokeTarget3") && expectedType.equals("InvokeTarget")) return true;
		if (type.equals("InvokeTarget1") && expectedType.equals("InvokeTarget")) return true;
		if (type.equals("InvokeTarget2") && expectedType.equals("InvokeTarget")) return true;
		return false;
	}
}
