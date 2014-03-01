package de.ovgu.cide.fstgen.parsers.generated_capprox;

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
		if (nonTerminal.getType().equals("TranslationUnit")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "Sequence_CodeUnit_TopLevel");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("Sequence_CodeUnit_TopLevel")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"CodeUnit_TopLevel")) {
				v.accept(this);
				hintNewLine();
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("StructDec")) {
			printFeatures(nonTerminal,true);
			printToken("struct");
			{
				FSTNode v=getChild(nonTerminal, "Id");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("{");
			for (FSTNode v : getChildren(nonTerminal,"Statement")) {
				v.accept(this);
			}
			printToken("}");
			printToken(";");
			printFeatures(nonTerminal,false);
			return false;
		}
		throw new RuntimeException("Unknown Non Terminal in FST "+nonTerminal);
	}
	protected boolean isSubtype(String type, String expectedType) {
		if (type.equals(expectedType)) return true;
		if (type.equals("VarDeclToken10") && expectedType.equals("VarDeclToken")) return true;
		if (type.equals("AnyStmtToken8") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("PPDefineStatement1") && expectedType.equals("PPDefineStatement")) return true;
		if (type.equals("AnyTypeDefToken1") && expectedType.equals("AnyTypeDefToken")) return true;
		if (type.equals("AnyStmtToken27") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("SwCaseLabel2") && expectedType.equals("SwCaseLabel")) return true;
		if (type.equals("VarDeclTokenOrComma2") && expectedType.equals("VarDeclTokenOrComma")) return true;
		if (type.equals("IfDefTL") && expectedType.equals("CodeUnit_TopLevel")) return true;
		if (type.equals("Func") && expectedType.equals("CodeUnit_TopLevel")) return true;
		if (type.equals("Modifier6") && expectedType.equals("Modifier")) return true;
		if (type.equals("BlockOrSingleStatement1") && expectedType.equals("BlockOrSingleStatement")) return true;
		if (type.equals("Modifier2") && expectedType.equals("Modifier")) return true;
		if (type.equals("AnyStmtToken4") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("AnyStmtToken17") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("AnyStmtToken23") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("IfElseIf1") && expectedType.equals("IfElseIf")) return true;
		if (type.equals("PreprocessorBL") && expectedType.equals("CodeUnit_InBlock")) return true;
		if (type.equals("For") && expectedType.equals("CodeUnit_InBlock")) return true;
		if (type.equals("AnyStmtToken13") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("SwCase1") && expectedType.equals("SwCase")) return true;
		if (type.equals("VarDeclToken7") && expectedType.equals("VarDeclToken")) return true;
		if (type.equals("PPOtherIgnore1") && expectedType.equals("PPOtherIgnore")) return true;
		if (type.equals("IfDefLine2") && expectedType.equals("IfDefLine")) return true;
		if (type.equals("BlockOrSemi2") && expectedType.equals("BlockOrSemi")) return true;
		if (type.equals("VarDeclToken3") && expectedType.equals("VarDeclToken")) return true;
		if (type.equals("TypeDef2") && expectedType.equals("TypeDef")) return true;
		if (type.equals("Define") && expectedType.equals("CodeUnit_TopLevel")) return true;
		if (type.equals("PPDefineStatement2") && expectedType.equals("PPDefineStatement")) return true;
		if (type.equals("VarDeclToken11") && expectedType.equals("VarDeclToken")) return true;
		if (type.equals("AnyStmtToken7") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("IfDefBL") && expectedType.equals("CodeUnit_InBlock")) return true;
		if (type.equals("Preprocessor") && expectedType.equals("CodeUnit_TopLevel")) return true;
		if (type.equals("AnyStmtToken28") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("AnyStmtToken10") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("ExternDec") && expectedType.equals("CodeUnit_TopLevel")) return true;
		if (type.equals("VarDeclTokenOrComma1") && expectedType.equals("VarDeclTokenOrComma")) return true;
		if (type.equals("If") && expectedType.equals("CodeUnit_InBlock")) return true;
		if (type.equals("Modifier3") && expectedType.equals("Modifier")) return true;
		if (type.equals("AnyStmtToken3") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("AnyStmtToken24") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("IfElseIf2") && expectedType.equals("IfElseIf")) return true;
		if (type.equals("AnyStmtToken30") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("AnyStmtToken18") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("Blck") && expectedType.equals("CodeUnit_InBlock")) return true;
		if (type.equals("TypeDef_") && expectedType.equals("CodeUnit_TopLevel")) return true;
		if (type.equals("AnyStmtToken14") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("SwCase2") && expectedType.equals("SwCase")) return true;
		if (type.equals("StructDec") && expectedType.equals("CodeUnit_TopLevel")) return true;
		if (type.equals("IfDefLine1") && expectedType.equals("IfDefLine")) return true;
		if (type.equals("VarDeclToken8") && expectedType.equals("VarDeclToken")) return true;
		if (type.equals("PPOtherIgnore2") && expectedType.equals("PPOtherIgnore")) return true;
		if (type.equals("AnyStmtToken20") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("Stmt") && expectedType.equals("CodeUnit_InBlock")) return true;
		if (type.equals("BlockOrSemi3") && expectedType.equals("BlockOrSemi")) return true;
		if (type.equals("VarDeclToken4") && expectedType.equals("VarDeclToken")) return true;
		if (type.equals("VarDeclToken1") && expectedType.equals("VarDeclToken")) return true;
		if (type.equals("VarDeclToken12") && expectedType.equals("VarDeclToken")) return true;
		if (type.equals("IncludeBL") && expectedType.equals("CodeUnit_InBlock")) return true;
		if (type.equals("PPOtherIgnore3") && expectedType.equals("PPOtherIgnore")) return true;
		if (type.equals("AnyStmtToken6") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("Modifier4") && expectedType.equals("Modifier")) return true;
		if (type.equals("AnyStmtToken11") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("AnyStmtToken29") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("AnyStmtToken2") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("AnyStmtToken25") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("DefineBL") && expectedType.equals("CodeUnit_InBlock")) return true;
		if (type.equals("AnyStmtToken19") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("While") && expectedType.equals("CodeUnit_InBlock")) return true;
		if (type.equals("AnyStmtToken21") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("VarDeclToken5") && expectedType.equals("VarDeclToken")) return true;
		if (type.equals("AnyStmtToken31") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("VarDeclToken9") && expectedType.equals("VarDeclToken")) return true;
		if (type.equals("AnyTypeDefToken2") && expectedType.equals("AnyTypeDefToken")) return true;
		if (type.equals("AnyStmtToken15") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("CodeUnit_InBlock10") && expectedType.equals("CodeUnit_InBlock")) return true;
		if (type.equals("SwCaseLabel1") && expectedType.equals("SwCaseLabel")) return true;
		if (type.equals("AnyStmtToken26") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("AnyStmtToken5") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("VarDeclToken2") && expectedType.equals("VarDeclToken")) return true;
		if (type.equals("AnyStmtToken1") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("Include") && expectedType.equals("CodeUnit_TopLevel")) return true;
		if (type.equals("Modifier5") && expectedType.equals("Modifier")) return true;
		if (type.equals("StmtTL") && expectedType.equals("CodeUnit_TopLevel")) return true;
		if (type.equals("BlockOrSingleStatement2") && expectedType.equals("BlockOrSingleStatement")) return true;
		if (type.equals("Switch") && expectedType.equals("CodeUnit_InBlock")) return true;
		if (type.equals("AnyStmtToken12") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("Modifier1") && expectedType.equals("Modifier")) return true;
		if (type.equals("VarDeclToken6") && expectedType.equals("VarDeclToken")) return true;
		if (type.equals("BlockOrSemi1") && expectedType.equals("BlockOrSemi")) return true;
		if (type.equals("AnyStmtToken22") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("AnyTypeDefToken3") && expectedType.equals("AnyTypeDefToken")) return true;
		if (type.equals("IfDefLine3") && expectedType.equals("IfDefLine")) return true;
		if (type.equals("TypeDef1") && expectedType.equals("TypeDef")) return true;
		if (type.equals("Do") && expectedType.equals("CodeUnit_InBlock")) return true;
		if (type.equals("AnyStmtToken16") && expectedType.equals("AnyStmtToken")) return true;
		if (type.equals("AnyStmtToken9") && expectedType.equals("AnyStmtToken")) return true;
		return false;
	}
}
