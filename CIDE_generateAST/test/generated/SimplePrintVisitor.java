package generated;

import java.util.*;
import cide.gast.*;

import java.io.PrintStream;

import cide.languages.*;

public class SimplePrintVisitor extends AbstractPrintVisitor implements ILanguagePrintVisitor {
	public SimplePrintVisitor(PrintStream out) {
		super(out);
	}
	public SimplePrintVisitor() {
		super();
	}
	public boolean visit(ASTNode node) {
		if (node instanceof ASTStringNode){
			printToken(((ASTStringNode)node).getValue());
			return false;
		}
		if (node instanceof ASTTextNode){
			return false;
		}
		if (node instanceof MethodDeclaration) {
			MethodDeclaration n = (MethodDeclaration)node;
			{
				Type v=n.getType();
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				ASTTextNode v=n.getText1();
				if (v!=null) {
					printToken("static");
					v.accept(this);
				}
			}
			{
				ASTStringNode v=n.getIdentifier();
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("(");
			{
				ParameterList v=n.getParameterList();
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken(")");
			{
				NameList v=n.getNameList();
				if (v!=null) {
					printToken("throws");
					v.accept(this);
				}
			}
			printToken("{");
			printToken("}");
			return false;
		}
		if (node instanceof Type1) {
			Type1 n = (Type1)node;
			printToken("void");
			return false;
		}
		if (node instanceof Type2) {
			Type2 n = (Type2)node;
			printToken("int");
			return false;
		}
		if (node instanceof ParameterList) {
			ParameterList n = (ParameterList)node;
			Iterator<Parameter> listElements = n.getParameter().iterator();
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				printToken(",");
				listElements.next().accept(this);
			}
			return false;
		}
		if (node instanceof Parameter) {
			Parameter n = (Parameter)node;
			{
				Type v=n.getType();
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				ASTStringNode v=n.getIdentifier();
				if (v!=null) {
					v.accept(this);
				}
			}
			return false;
		}
		if (node instanceof NameList) {
			NameList n = (NameList)node;
			Iterator<Name> listElements = n.getName().iterator();
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				printToken(",");
				listElements.next().accept(this);
			}
			return false;
		}
		if (node instanceof Name) {
			Name n = (Name)node;
			{
				ASTStringNode v=n.getIdentifier();
				if (v!=null) {
					v.accept(this);
				}
			}
			return false;
		}
		if (node instanceof Statement1) {
			Statement1 n = (Statement1)node;
			{
				ASTStringNode v=n.getIdentifier();
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken(";");
			return false;
		}
		if (node instanceof Statement2) {
			Statement2 n = (Statement2)node;
			{
				ASTNode v=n.getIfStatement();
				if (v!=null) {
					v.accept(this);
				}
			}
			return false;
		}
		if (node instanceof IfStatement) {
			IfStatement n = (IfStatement)node;
			printToken("if");
			printToken("(");
			{
				ASTStringNode v=n.getExpression();
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken(")");
			{
				Statement v=n.getStatement();
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				Statement v=n.getStatement1();
				if (v!=null) {
					printToken("else");
					v.accept(this);
				}
			}
			return false;
		}
		if (node instanceof Production) {
			Production n = (Production)node;
			{
				ASTStringNode v=n.getIdentifier();
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken(":");
			{
				Units v=n.getUnits();
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken(";");
			return false;
		}
		if (node instanceof Units) {
			Units n = (Units)node;
			for (Unit v : n.getUnit()) {
				v.accept(this);
			}
			return false;
		}
		if (node instanceof Unit1) {
			Unit1 n = (Unit1)node;
			{
				ASTStringNode v=n.getString_literal();
				if (v!=null) {
					v.accept(this);
				}
			}
			return false;
		}
		if (node instanceof Unit2) {
			Unit2 n = (Unit2)node;
			{
				NonTerminal v=n.getNonTerminal();
				if (v!=null) {
					v.accept(this);
				}
			}
			return false;
		}
		if (node instanceof NonTerminal) {
			NonTerminal n = (NonTerminal)node;
			{
				ASTStringNode v=n.getIdentifier();
				if (v!=null) {
					v.accept(this);
				}
			}
			return false;
		}
		return true;
	}
}
