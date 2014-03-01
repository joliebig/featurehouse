package de.ovgu.cide.fstgen.parsers.generated_haskell;

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
		if (nonTerminal.getType().equals("module")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "moduleHeader");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("{");
			hintIncIndent();
			hintNewLine();
			{
				FSTNode v=getChild(nonTerminal, "body");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintDecIndent();
			hintNewLine();
			printToken("}");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("body")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "imports");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "definitions");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("imports")) {
			printFeatures(nonTerminal,true);
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "importDecl").iterator();
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				printToken(";");
				hintNewLine();
				hintNewLine();
				listElements.next().accept(this);
			}
			printToken(";");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("moduleHeader")) {
			printFeatures(nonTerminal,true);
			printToken("module");
			{
				FSTNode v=getChild(nonTerminal, "naam");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "exports");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("where");
			hintNewLine();
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("exports")) {
			printFeatures(nonTerminal,true);
			printToken("(");
			{
				FSTNode v=getChild(nonTerminal, "exportList");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken(")");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("exportList")) {
			printFeatures(nonTerminal,true);
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "export").iterator();
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				printToken(",");
				listElements.next().accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("definitions")) {
			printFeatures(nonTerminal,true);
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "definition").iterator();
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				printToken(";");
				hintNewLine();
				hintNewLine();
				listElements.next().accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("typedecl")) {
			printFeatures(nonTerminal,true);
			printToken("type");
			{
				FSTNode v=getChild(nonTerminal, "simpletype");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("=");
			{
				FSTNode v=getChild(nonTerminal, "functiontype");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("datadecl")) {
			printFeatures(nonTerminal,true);
			printToken("data");
			{
				FSTNode v=getChild(nonTerminal, "context");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "simpletype");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("=");
			{
				FSTNode v=getChild(nonTerminal, "constrs");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "deriving");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("newtypedecl")) {
			printFeatures(nonTerminal,true);
			printToken("newtype");
			{
				FSTNode v=getChild(nonTerminal, "context");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "simpletype");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("=");
			{
				FSTNode v=getChild(nonTerminal, "naam");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "newtypeParam");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "deriving");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("classdecl")) {
			printFeatures(nonTerminal,true);
			printToken("class");
			{
				FSTNode v=getChild(nonTerminal, "context");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "naam");
				if (v!=null) {
					v.accept(this);
				}
			}
			for (FSTNode v : getChildren(nonTerminal,"var")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "whereDecls");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("instancedecl")) {
			printFeatures(nonTerminal,true);
			printToken("instance");
			{
				FSTNode v=getChild(nonTerminal, "context");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "inst");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "whereDecls");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("defaultdecl")) {
			printFeatures(nonTerminal,true);
			printToken("default");
			{
				FSTNode v=getChild(nonTerminal, "functiontypeList");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("constrs")) {
			printFeatures(nonTerminal,true);
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "constr").iterator();
			hintIncIndent();
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				printToken("|");
				hintNewLine();
				listElements.next().accept(this);
			}
			hintDecIndent();
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("decls")) {
			printFeatures(nonTerminal,true);
			printToken("{");
			hintIncIndent();
			{
				FSTNode v=getChild(nonTerminal, "declarationList");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintDecIndent();
			printToken("}");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("declarationList")) {
			printFeatures(nonTerminal,true);
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "declaration").iterator();
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				printToken(";");
				hintNewLine();
				listElements.next().accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("whereDecls")) {
			printFeatures(nonTerminal,true);
			printToken("where");
			hintIncIndent();
			hintNewLine();
			{
				FSTNode v=getChild(nonTerminal, "decls");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintDecIndent();
			printFeatures(nonTerminal,false);
			return false;
		}
		throw new RuntimeException("Unknown Non Terminal in FST "+nonTerminal);
	}
	protected boolean isSubtype(String type, String expectedType) {
		if (type.equals(expectedType)) return true;
		if (type.equals("altSpecial1") && expectedType.equals("altSpecial")) return true;
		if (type.equals("patroonPrefix1") && expectedType.equals("patroonPrefix")) return true;
		if (type.equals("otherOperators1") && expectedType.equals("otherOperators")) return true;
		if (type.equals("varsym2") && expectedType.equals("varsym")) return true;
		if (type.equals("qop2") && expectedType.equals("qop")) return true;
		if (type.equals("patrMain1") && expectedType.equals("patrMain")) return true;
		if (type.equals("op1") && expectedType.equals("op")) return true;
		if (type.equals("export2") && expectedType.equals("export")) return true;
		if (type.equals("patroonMain7") && expectedType.equals("patroonMain")) return true;
		if (type.equals("exprMain7") && expectedType.equals("exprMain")) return true;
		if (type.equals("qconop1") && expectedType.equals("qconop")) return true;
		if (type.equals("exprListSpecial2") && expectedType.equals("exprListSpecial")) return true;
		if (type.equals("newtypeParam2") && expectedType.equals("newtypeParam")) return true;
		if (type.equals("conop1") && expectedType.equals("conop")) return true;
		if (type.equals("datadecl") && expectedType.equals("definition")) return true;
		if (type.equals("deriving2") && expectedType.equals("deriving")) return true;
		if (type.equals("declaration5") && expectedType.equals("declaration")) return true;
		if (type.equals("patroonPrefix2") && expectedType.equals("patroonPrefix")) return true;
		if (type.equals("qop3") && expectedType.equals("qop")) return true;
		if (type.equals("expressie2") && expectedType.equals("expressie")) return true;
		if (type.equals("op2") && expectedType.equals("op")) return true;
		if (type.equals("qop1") && expectedType.equals("qop")) return true;
		if (type.equals("otherOperators2") && expectedType.equals("otherOperators")) return true;
		if (type.equals("classdecl") && expectedType.equals("definition")) return true;
		if (type.equals("altSpecial2") && expectedType.equals("altSpecial")) return true;
		if (type.equals("export3") && expectedType.equals("export")) return true;
		if (type.equals("exprMain6") && expectedType.equals("exprMain")) return true;
		if (type.equals("qconop2") && expectedType.equals("qconop")) return true;
		if (type.equals("exprListSpecial3") && expectedType.equals("exprListSpecial")) return true;
		if (type.equals("newtypeParam1") && expectedType.equals("newtypeParam")) return true;
		if (type.equals("conop2") && expectedType.equals("conop")) return true;
		if (type.equals("declaration") && expectedType.equals("definition")) return true;
		if (type.equals("deriving1") && expectedType.equals("deriving")) return true;
		if (type.equals("declaration4") && expectedType.equals("declaration")) return true;
		if (type.equals("expressie3") && expectedType.equals("expressie")) return true;
		if (type.equals("context3") && expectedType.equals("context")) return true;
		if (type.equals("patroonMain5") && expectedType.equals("patroonMain")) return true;
		if (type.equals("varop1") && expectedType.equals("varop")) return true;
		if (type.equals("patrMain3") && expectedType.equals("patrMain")) return true;
		if (type.equals("otherOperators3") && expectedType.equals("otherOperators")) return true;
		if (type.equals("expressie8") && expectedType.equals("expressie")) return true;
		if (type.equals("altSpecial3") && expectedType.equals("altSpecial")) return true;
		if (type.equals("constr2") && expectedType.equals("constr")) return true;
		if (type.equals("qcon1") && expectedType.equals("qcon")) return true;
		if (type.equals("newtypedecl") && expectedType.equals("definition")) return true;
		if (type.equals("naamOrVar1") && expectedType.equals("naamOrVar")) return true;
		if (type.equals("context2") && expectedType.equals("context")) return true;
		if (type.equals("declaration7") && expectedType.equals("declaration")) return true;
		if (type.equals("gconsym2") && expectedType.equals("gconsym")) return true;
		if (type.equals("patrMain2") && expectedType.equals("patrMain")) return true;
		if (type.equals("patroonMain6") && expectedType.equals("patroonMain")) return true;
		if (type.equals("type1") && expectedType.equals("type")) return true;
		if (type.equals("constr3") && expectedType.equals("constr")) return true;
		if (type.equals("naamOrVar2") && expectedType.equals("naamOrVar")) return true;
		if (type.equals("exprListSpecial1") && expectedType.equals("exprListSpecial")) return true;
		if (type.equals("otherOperators4") && expectedType.equals("otherOperators")) return true;
		if (type.equals("expressie1") && expectedType.equals("expressie")) return true;
		if (type.equals("context1") && expectedType.equals("context")) return true;
		if (type.equals("declaration6") && expectedType.equals("declaration")) return true;
		if (type.equals("defaultdecl") && expectedType.equals("definition")) return true;
		if (type.equals("gconsym1") && expectedType.equals("gconsym")) return true;
		if (type.equals("constructorNaam") && expectedType.equals("patroonMain")) return true;
		if (type.equals("type2") && expectedType.equals("type")) return true;
		if (type.equals("function3") && expectedType.equals("function")) return true;
		if (type.equals("literal2") && expectedType.equals("literal")) return true;
		if (type.equals("exprMain1") && expectedType.equals("exprMain")) return true;
		if (type.equals("constr1") && expectedType.equals("constr")) return true;
		if (type.equals("qvarop1") && expectedType.equals("qvarop")) return true;
		if (type.equals("qvarop2") && expectedType.equals("qvarop")) return true;
		if (type.equals("expressie6") && expectedType.equals("expressie")) return true;
		if (type.equals("declaration1") && expectedType.equals("declaration")) return true;
		if (type.equals("type3") && expectedType.equals("type")) return true;
		if (type.equals("declaration8") && expectedType.equals("declaration")) return true;
		if (type.equals("literal1") && expectedType.equals("literal")) return true;
		if (type.equals("varMain2") && expectedType.equals("varMain")) return true;
		if (type.equals("caseInner1") && expectedType.equals("caseInner")) return true;
		if (type.equals("exprMain2") && expectedType.equals("exprMain")) return true;
		if (type.equals("exprMain3") && expectedType.equals("exprMain")) return true;
		if (type.equals("expressie7") && expectedType.equals("expressie")) return true;
		if (type.equals("varop2") && expectedType.equals("varop")) return true;
		if (type.equals("patroonMain3") && expectedType.equals("patroonMain")) return true;
		if (type.equals("exportNaamParam1") && expectedType.equals("exportNaamParam")) return true;
		if (type.equals("type4") && expectedType.equals("type")) return true;
		if (type.equals("klasseTypeVar2") && expectedType.equals("klasseTypeVar")) return true;
		if (type.equals("literal4") && expectedType.equals("literal")) return true;
		if (type.equals("caseInner2") && expectedType.equals("caseInner")) return true;
		if (type.equals("varMain1") && expectedType.equals("varMain")) return true;
		if (type.equals("function1") && expectedType.equals("function")) return true;
		if (type.equals("expressie4") && expectedType.equals("expressie")) return true;
		if (type.equals("exprMain4") && expectedType.equals("exprMain")) return true;
		if (type.equals("declaration3") && expectedType.equals("declaration")) return true;
		if (type.equals("typedecl") && expectedType.equals("definition")) return true;
		if (type.equals("patroonMain2") && expectedType.equals("patroonMain")) return true;
		if (type.equals("qcon2") && expectedType.equals("qcon")) return true;
		if (type.equals("op3") && expectedType.equals("op")) return true;
		if (type.equals("exportNaamParam2") && expectedType.equals("exportNaamParam")) return true;
		if (type.equals("type5") && expectedType.equals("type")) return true;
		if (type.equals("klasseTypeVar1") && expectedType.equals("klasseTypeVar")) return true;
		if (type.equals("literal3") && expectedType.equals("literal")) return true;
		if (type.equals("function2") && expectedType.equals("function")) return true;
		if (type.equals("varsym1") && expectedType.equals("varsym")) return true;
		if (type.equals("exprMain5") && expectedType.equals("exprMain")) return true;
		if (type.equals("instancedecl") && expectedType.equals("definition")) return true;
		if (type.equals("declaration2") && expectedType.equals("declaration")) return true;
		if (type.equals("export1") && expectedType.equals("export")) return true;
		if (type.equals("patroonMain1") && expectedType.equals("patroonMain")) return true;
		if (type.equals("expressie5") && expectedType.equals("expressie")) return true;
		return false;
	}
}
