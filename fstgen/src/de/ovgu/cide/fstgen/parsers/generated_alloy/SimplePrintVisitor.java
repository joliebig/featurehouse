package de.ovgu.cide.fstgen.parsers.generated_alloy;

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
		if (nonTerminal.getType().equals("Specification")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "Module");
				if (v!=null) {
					v.accept(this);
				}
			}
			for (FSTNode v : getChildren(nonTerminal,"Open")) {
				v.accept(this);
			}
			for (FSTNode v : getChildren(nonTerminal,"Paragraph")) {
				v.accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Module")) {
			printFeatures(nonTerminal,true);
			printToken("module");
			{
				FSTNode v=getChild(nonTerminal, "Name");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "ExactlyClause");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Open")) {
			printFeatures(nonTerminal,true);
			printToken("open");
			{
				FSTNode v=getChild(nonTerminal, "Name");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "RefClause");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "AsClause");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Paragraph5")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "EnumDecl");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Paragraph6")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "SigDecl");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("NameOrBlock1")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "NonEmptyName");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("NameOrBlock2")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "Block");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("SigDecl")) {
			printFeatures(nonTerminal,true);
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "Name").iterator();
			for (FSTNode v : getChildren(nonTerminal,"SigQual")) {
				v.accept(this);
			}
			printToken("sig");
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				printToken(",");
				listElements.next().accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "SigExt");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("{");
			{
				FSTNode v=getChild(nonTerminal, "SigBody");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("}");
			{
				FSTNode v=getChild(nonTerminal, "Block");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("SigBody")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "Decls");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Decls")) {
			printFeatures(nonTerminal,true);
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "Decl").iterator();
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
		if (nonTerminal.getType().equals("EnumDecl")) {
			printFeatures(nonTerminal,true);
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "Name").iterator();
			printToken("enum");
			{
				FSTNode v=getChild(nonTerminal, "Name");
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
			printToken("}");
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Block")) {
			printFeatures(nonTerminal,true);
			printToken("{");
			for (FSTNode v : getChildren(nonTerminal,"Expr")) {
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
		if (type.equals("Paragraph2") && expectedType.equals("Paragraph")) return true;
		if (type.equals("BinOp13") && expectedType.equals("BinOp")) return true;
		if (type.equals("NotClause1") && expectedType.equals("NotClause")) return true;
		if (type.equals("Expr513") && expectedType.equals("Expr5")) return true;
		if (type.equals("BinOp7") && expectedType.equals("BinOp")) return true;
		if (type.equals("CompareOp5") && expectedType.equals("CompareOp")) return true;
		if (type.equals("Quant3") && expectedType.equals("Quant")) return true;
		if (type.equals("ImpliesClause2") && expectedType.equals("ImpliesClause")) return true;
		if (type.equals("UnOp7") && expectedType.equals("UnOp")) return true;
		if (type.equals("Ref2") && expectedType.equals("Ref")) return true;
		if (type.equals("NameClause1") && expectedType.equals("NameClause")) return true;
		if (type.equals("Expr53") && expectedType.equals("Expr5")) return true;
		if (type.equals("FunDecl4") && expectedType.equals("FunDecl")) return true;
		if (type.equals("ArrowOpClause2") && expectedType.equals("ArrowOpClause")) return true;
		if (type.equals("NonEmptyNameClause1") && expectedType.equals("NonEmptyNameClause")) return true;
		if (type.equals("Expr512") && expectedType.equals("Expr5")) return true;
		if (type.equals("BinOp8") && expectedType.equals("BinOp")) return true;
		if (type.equals("BinOp12") && expectedType.equals("BinOp")) return true;
		if (type.equals("UnOp6") && expectedType.equals("UnOp")) return true;
		if (type.equals("CompareOp4") && expectedType.equals("CompareOp")) return true;
		if (type.equals("Paragraph3") && expectedType.equals("Paragraph")) return true;
		if (type.equals("BlockOrBar2") && expectedType.equals("BlockOrBar")) return true;
		if (type.equals("Ref3") && expectedType.equals("Ref")) return true;
		if (type.equals("ImpliesClause1") && expectedType.equals("ImpliesClause")) return true;
		if (type.equals("Quant4") && expectedType.equals("Quant")) return true;
		if (type.equals("Ref4") && expectedType.equals("Ref")) return true;
		if (type.equals("Expr52") && expectedType.equals("Expr5")) return true;
		if (type.equals("SigQual5") && expectedType.equals("SigQual")) return true;
		if (type.equals("FunDecl3") && expectedType.equals("FunDecl")) return true;
		if (type.equals("ArrowOpClause3") && expectedType.equals("ArrowOpClause")) return true;
		if (type.equals("NonEmptyNameClause2") && expectedType.equals("NonEmptyNameClause")) return true;
		if (type.equals("Paragraph4") && expectedType.equals("Paragraph")) return true;
		if (type.equals("UnOp5") && expectedType.equals("UnOp")) return true;
		if (type.equals("BinOp15") && expectedType.equals("BinOp")) return true;
		if (type.equals("Quant1") && expectedType.equals("Quant")) return true;
		if (type.equals("BlockOrBar1") && expectedType.equals("BlockOrBar")) return true;
		if (type.equals("Scope2") && expectedType.equals("Scope")) return true;
		if (type.equals("NameClause3") && expectedType.equals("NameClause")) return true;
		if (type.equals("FunDecl2") && expectedType.equals("FunDecl")) return true;
		if (type.equals("UnOp11") && expectedType.equals("UnOp")) return true;
		if (type.equals("BinOp1") && expectedType.equals("BinOp")) return true;
		if (type.equals("Expr55") && expectedType.equals("Expr5")) return true;
		if (type.equals("SigQual4") && expectedType.equals("SigQual")) return true;
		if (type.equals("BinOp9") && expectedType.equals("BinOp")) return true;
		if (type.equals("SigExt1") && expectedType.equals("SigExt")) return true;
		if (type.equals("CompareOp6") && expectedType.equals("CompareOp")) return true;
		if (type.equals("Paragraph5") && expectedType.equals("Paragraph")) return true;
		if (type.equals("BinOp14") && expectedType.equals("BinOp")) return true;
		if (type.equals("NotClause2") && expectedType.equals("NotClause")) return true;
		if (type.equals("UnOp4") && expectedType.equals("UnOp")) return true;
		if (type.equals("Expr514") && expectedType.equals("Expr5")) return true;
		if (type.equals("Quant2") && expectedType.equals("Quant")) return true;
		if (type.equals("BinOp2") && expectedType.equals("BinOp")) return true;
		if (type.equals("ArrowOpClause1") && expectedType.equals("ArrowOpClause")) return true;
		if (type.equals("UnOp10") && expectedType.equals("UnOp")) return true;
		if (type.equals("Expr54") && expectedType.equals("Expr5")) return true;
		if (type.equals("NameClause2") && expectedType.equals("NameClause")) return true;
		if (type.equals("SigQual3") && expectedType.equals("SigQual")) return true;
		if (type.equals("FunDecl1") && expectedType.equals("FunDecl")) return true;
		if (type.equals("BinOp17") && expectedType.equals("BinOp")) return true;
		if (type.equals("Expr57") && expectedType.equals("Expr5")) return true;
		if (type.equals("TypeScopeClause1") && expectedType.equals("TypeScopeClause")) return true;
		if (type.equals("SigQual1") && expectedType.equals("SigQual")) return true;
		if (type.equals("BinOp3") && expectedType.equals("BinOp")) return true;
		if (type.equals("UnOp3") && expectedType.equals("UnOp")) return true;
		if (type.equals("CompareOp1") && expectedType.equals("CompareOp")) return true;
		if (type.equals("Paragraph6") && expectedType.equals("Paragraph")) return true;
		if (type.equals("TypeScopeClause2") && expectedType.equals("TypeScopeClause")) return true;
		if (type.equals("BinOp16") && expectedType.equals("BinOp")) return true;
		if (type.equals("Expr56") && expectedType.equals("Expr5")) return true;
		if (type.equals("UnOp12") && expectedType.equals("UnOp")) return true;
		if (type.equals("BinOp4") && expectedType.equals("BinOp")) return true;
		if (type.equals("SigQual2") && expectedType.equals("SigQual")) return true;
		if (type.equals("Scope1") && expectedType.equals("Scope")) return true;
		if (type.equals("NameOrBlock2") && expectedType.equals("NameOrBlock")) return true;
		if (type.equals("UnOp2") && expectedType.equals("UnOp")) return true;
		if (type.equals("RunOrCheck1") && expectedType.equals("RunOrCheck")) return true;
		if (type.equals("Expr59") && expectedType.equals("Expr5")) return true;
		if (type.equals("UnOp9") && expectedType.equals("UnOp")) return true;
		if (type.equals("TypeScopeClause3") && expectedType.equals("TypeScopeClause")) return true;
		if (type.equals("Expr511") && expectedType.equals("Expr5")) return true;
		if (type.equals("BinOp5") && expectedType.equals("BinOp")) return true;
		if (type.equals("FunDecl6") && expectedType.equals("FunDecl")) return true;
		if (type.equals("NameOrBlock1") && expectedType.equals("NameOrBlock")) return true;
		if (type.equals("ArrowOpClause4") && expectedType.equals("ArrowOpClause")) return true;
		if (type.equals("RunOrCheck2") && expectedType.equals("RunOrCheck")) return true;
		if (type.equals("CompareOp3") && expectedType.equals("CompareOp")) return true;
		if (type.equals("Expr51") && expectedType.equals("Expr5")) return true;
		if (type.equals("UnOp1") && expectedType.equals("UnOp")) return true;
		if (type.equals("Integer1") && expectedType.equals("Integer")) return true;
		if (type.equals("Quant5") && expectedType.equals("Quant")) return true;
		if (type.equals("BinOp10") && expectedType.equals("BinOp")) return true;
		if (type.equals("UnOp8") && expectedType.equals("UnOp")) return true;
		if (type.equals("BinOp18") && expectedType.equals("BinOp")) return true;
		if (type.equals("TypeScopeClause4") && expectedType.equals("TypeScopeClause")) return true;
		if (type.equals("Expr510") && expectedType.equals("Expr5")) return true;
		if (type.equals("FunDecl5") && expectedType.equals("FunDecl")) return true;
		if (type.equals("Expr58") && expectedType.equals("Expr5")) return true;
		if (type.equals("Paragraph1") && expectedType.equals("Paragraph")) return true;
		if (type.equals("BinOp6") && expectedType.equals("BinOp")) return true;
		if (type.equals("Ref1") && expectedType.equals("Ref")) return true;
		if (type.equals("SigExt2") && expectedType.equals("SigExt")) return true;
		if (type.equals("BinOp11") && expectedType.equals("BinOp")) return true;
		if (type.equals("CompareOp2") && expectedType.equals("CompareOp")) return true;
		if (type.equals("Integer2") && expectedType.equals("Integer")) return true;
		if (type.equals("Quant6") && expectedType.equals("Quant")) return true;
		return false;
	}
}
