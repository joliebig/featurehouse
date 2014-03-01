package de.ovgu.cide.fstgen.parsers.generated_sdf;

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
		if (nonTerminal.getType().equals("Module")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "ModuleDeclaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintNewLine();
			hintNewLine();
			{
				FSTNode v=getChild(nonTerminal, "ImportDeclaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintDecIndent();
			hintNewLine();
			{
				FSTNode v=getChild(nonTerminal, "ExportDeclaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "HiddenDeclaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("ImportDeclaration")) {
			printFeatures(nonTerminal,true);
			printToken("imports");
			hintIncIndent();
			for (FSTNode v : getChildren(nonTerminal,"ModName")) {
				v.accept(this);
			}
			hintNewLine();
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("ExportDeclaration")) {
			printFeatures(nonTerminal,true);
			printToken("exports");
			hintNewLine();
			hintIncIndent();
			hintNewLine();
			for (FSTNode v : getChildren(nonTerminal,"Grammar")) {
				v.accept(this);
			}
			hintDecIndent();
			hintNewLine();
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("HiddenDeclaration")) {
			printFeatures(nonTerminal,true);
			printToken("hiddens");
			hintNewLine();
			hintIncIndent();
			hintNewLine();
			for (FSTNode v : getChildren(nonTerminal,"Grammar")) {
				v.accept(this);
			}
			hintDecIndent();
			hintNewLine();
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Grammar1")) {
			printFeatures(nonTerminal,true);
			printToken("sorts");
			hintIncIndent();
			for (FSTNode v : getChildren(nonTerminal,"Sort")) {
				v.accept(this);
			}
			hintDecIndent();
			hintNewLine();
			hintNewLine();
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Grammar2")) {
			printFeatures(nonTerminal,true);
			printToken("context-free start-symbols");
			hintIncIndent();
			for (FSTNode v : getChildren(nonTerminal,"Sort")) {
				v.accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Grammar3")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "Productions");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Grammar4")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "Disambiguations");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Productions")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "ProdPart");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintIncIndent();
			for (FSTNode v : getChildren(nonTerminal,"Production")) {
				v.accept(this);
			}
			hintDecIndent();
			hintNewLine();
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Disambiguations1")) {
			printFeatures(nonTerminal,true);
			printToken("context-free priorities");
			for (FSTNode v : getChildren(nonTerminal,"Priority")) {
				v.accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Disambiguations2")) {
			printFeatures(nonTerminal,true);
			printToken("lexical restrictions");
			for (FSTNode v : getChildren(nonTerminal,"Restriction")) {
				v.accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Disambiguations3")) {
			printFeatures(nonTerminal,true);
			printToken("context-free restrictions");
			for (FSTNode v : getChildren(nonTerminal,"Restriction")) {
				v.accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		throw new RuntimeException("Unknown Non Terminal in FST "+nonTerminal);
	}
	protected boolean isSubtype(String type, String expectedType) {
		if (type.equals(expectedType)) return true;
		if (type.equals("SpecialSign14") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Attribute2") && expectedType.equals("Attribute")) return true;
		if (type.equals("SortOp1") && expectedType.equals("SortOp")) return true;
		if (type.equals("ProdPart4") && expectedType.equals("ProdPart")) return true;
		if (type.equals("CharClass3") && expectedType.equals("CharClass")) return true;
		if (type.equals("SpecialSign18") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Attribute6") && expectedType.equals("Attribute")) return true;
		if (type.equals("SpecialSign28") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("SpecialSign24") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Attribute11") && expectedType.equals("Attribute")) return true;
		if (type.equals("SpecialSign20") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("SpecialSign2") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Symbol2") && expectedType.equals("Symbol")) return true;
		if (type.equals("Symbol1") && expectedType.equals("Symbol")) return true;
		if (type.equals("SpecialSign8") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Disambiguations2") && expectedType.equals("Disambiguations")) return true;
		if (type.equals("CharClass2") && expectedType.equals("CharClass")) return true;
		if (type.equals("Sort4") && expectedType.equals("Sort")) return true;
		if (type.equals("SpecialSign10") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("CharClass4") && expectedType.equals("CharClass")) return true;
		if (type.equals("Attribute3") && expectedType.equals("Attribute")) return true;
		if (type.equals("ProdPart5") && expectedType.equals("ProdPart")) return true;
		if (type.equals("SpecialSign15") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("SpecialSign19") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Priority1") && expectedType.equals("Priority")) return true;
		if (type.equals("Attribute7") && expectedType.equals("Attribute")) return true;
		if (type.equals("SpecialSign6") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("SpecialSign29") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("ModuleSort1") && expectedType.equals("ModuleSort")) return true;
		if (type.equals("Grammar1") && expectedType.equals("Grammar")) return true;
		if (type.equals("SpecialSign25") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Attribute10") && expectedType.equals("Attribute")) return true;
		if (type.equals("ProdPart1") && expectedType.equals("ProdPart")) return true;
		if (type.equals("SpecialSign21") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Grammar4") && expectedType.equals("Grammar")) return true;
		if (type.equals("Symbol3") && expectedType.equals("Symbol")) return true;
		if (type.equals("SpecialSign3") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("SpecialSign7") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Sort1") && expectedType.equals("Sort")) return true;
		if (type.equals("Disambiguations1") && expectedType.equals("Disambiguations")) return true;
		if (type.equals("SpecialSign11") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Sort5") && expectedType.equals("Sort")) return true;
		if (type.equals("CharClass1") && expectedType.equals("CharClass")) return true;
		if (type.equals("CFOrLEX1") && expectedType.equals("CFOrLEX")) return true;
		if (type.equals("SpecialSign12") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Sort2") && expectedType.equals("Sort")) return true;
		if (type.equals("SortOp3") && expectedType.equals("SortOp")) return true;
		if (type.equals("ProdPart2") && expectedType.equals("ProdPart")) return true;
		if (type.equals("Priority2") && expectedType.equals("Priority")) return true;
		if (type.equals("SpecialSign26") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Constructor1") && expectedType.equals("Constructor")) return true;
		if (type.equals("Attribute4") && expectedType.equals("Attribute")) return true;
		if (type.equals("SpecialSign5") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("SpecialSign16") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Symbol4") && expectedType.equals("Symbol")) return true;
		if (type.equals("Grammar3") && expectedType.equals("Grammar")) return true;
		if (type.equals("SpecialSign22") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("ModuleSort2") && expectedType.equals("ModuleSort")) return true;
		if (type.equals("Attribute8") && expectedType.equals("Attribute")) return true;
		if (type.equals("Attribute5") && expectedType.equals("Attribute")) return true;
		if (type.equals("SpecialSign13") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Attribute1") && expectedType.equals("Attribute")) return true;
		if (type.equals("SortOp2") && expectedType.equals("SortOp")) return true;
		if (type.equals("Constructor2") && expectedType.equals("Constructor")) return true;
		if (type.equals("SpecialSign27") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("SpecialSign17") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("ProdPart3") && expectedType.equals("ProdPart")) return true;
		if (type.equals("SpecialSign4") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("CFOrLEX2") && expectedType.equals("CFOrLEX")) return true;
		if (type.equals("Disambiguations3") && expectedType.equals("Disambiguations")) return true;
		if (type.equals("SpecialSign1") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Grammar2") && expectedType.equals("Grammar")) return true;
		if (type.equals("SpecialSign23") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Sort3") && expectedType.equals("Sort")) return true;
		if (type.equals("SpecialSign9") && expectedType.equals("SpecialSign")) return true;
		if (type.equals("Attribute9") && expectedType.equals("Attribute")) return true;
		return false;
	}
}
