package de.ovgu.cide.fstgen.parsers.generated_python;

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
		if (nonTerminal.getType().equals("file_input")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"stmt")) {
				v.accept(this);
				hintNewLine();
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("stmt1")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "simple_stmt");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("ClassDefinition")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "classdef");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("simple_stmt")) {
			printFeatures(nonTerminal,true);
			Iterator<FSTNode> listElements = getChildren(nonTerminal, "small_stmt").iterator();
			if (listElements.hasNext()) {
				listElements.next().accept(this);
			}
			while (listElements.hasNext()) {
				printToken(";");
				listElements.next().accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("classdef")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "decorators");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("class");
			{
				FSTNode v=getChild(nonTerminal, "ClassName");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "testlist");
				if (v!=null) {
					printToken("(");
					v.accept(this);
					printToken(")");
				}
			}
			printToken(":");
			hintNewLine();
			for (FSTNode v : getChildren(nonTerminal,"classdef_End")) {
				v.accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("classdef_End2")) {
			printFeatures(nonTerminal,true);
			hintIncIndent();
			for (FSTNode v : getChildren(nonTerminal,"classsimple_stmt")) {
				v.accept(this);
			}
			hintDecIndent();
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("classsimple_stmt")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "classsmall_stmt");
				if (v!=null) {
					v.accept(this);
				}
			}
			hintNewLine();
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("classsmall_stmt2")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "classdef");
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
		if (type.equals("funcdef_End1") && expectedType.equals("funcdef_End")) return true;
		if (type.equals("ImportFromEnd1") && expectedType.equals("ImportFromEnd")) return true;
		if (type.equals("classsmall_stmt5") && expectedType.equals("classsmall_stmt")) return true;
		if (type.equals("expr_stmtEnd10") && expectedType.equals("expr_stmtEnd")) return true;
		if (type.equals("AnyName9") && expectedType.equals("AnyName")) return true;
		if (type.equals("classsmall_stmt15") && expectedType.equals("classsmall_stmt")) return true;
		if (type.equals("atom2") && expectedType.equals("atom")) return true;
		if (type.equals("StringNode6") && expectedType.equals("StringNode")) return true;
		if (type.equals("AnyName18") && expectedType.equals("AnyName")) return true;
		if (type.equals("else_stmt_End1") && expectedType.equals("else_stmt_End")) return true;
		if (type.equals("while_stmt_End2") && expectedType.equals("while_stmt_End")) return true;
		if (type.equals("varargslist2") && expectedType.equals("varargslist")) return true;
		if (type.equals("stmt1") && expectedType.equals("stmt")) return true;
		if (type.equals("AnyName30") && expectedType.equals("AnyName")) return true;
		if (type.equals("classdef_End2") && expectedType.equals("classdef_End")) return true;
		if (type.equals("expr_stmtEnd7") && expectedType.equals("expr_stmtEnd")) return true;
		if (type.equals("varargslist3") && expectedType.equals("varargslist")) return true;
		if (type.equals("fpdef1") && expectedType.equals("fpdef")) return true;
		if (type.equals("comp_op5") && expectedType.equals("comp_op")) return true;
		if (type.equals("small_stmt1") && expectedType.equals("small_stmt")) return true;
		if (type.equals("AnyName23") && expectedType.equals("AnyName")) return true;
		if (type.equals("except_clause_test_End1") && expectedType.equals("except_clause_test_End")) return true;
		if (type.equals("arglist1EndEnd2") && expectedType.equals("arglist1EndEnd")) return true;
		if (type.equals("atom1") && expectedType.equals("atom")) return true;
		if (type.equals("arglist1EndEnd1") && expectedType.equals("arglist1EndEnd")) return true;
		if (type.equals("not_test1") && expectedType.equals("not_test")) return true;
		if (type.equals("classsmall_stmt14") && expectedType.equals("classsmall_stmt")) return true;
		if (type.equals("classsmall_stmt4") && expectedType.equals("classsmall_stmt")) return true;
		if (type.equals("flow_stmt1") && expectedType.equals("flow_stmt")) return true;
		if (type.equals("atomtrailerEnd1") && expectedType.equals("atomtrailerEnd")) return true;
		if (type.equals("StringNode5") && expectedType.equals("StringNode")) return true;
		if (type.equals("AnyName17") && expectedType.equals("AnyName")) return true;
		if (type.equals("arglist2") && expectedType.equals("arglist")) return true;
		if (type.equals("decorator_End1") && expectedType.equals("decorator_End")) return true;
		if (type.equals("AnyName1") && expectedType.equals("AnyName")) return true;
		if (type.equals("varargslist1") && expectedType.equals("varargslist")) return true;
		if (type.equals("while_stmt_End1") && expectedType.equals("while_stmt_End")) return true;
		if (type.equals("expr_stmtEnd8") && expectedType.equals("expr_stmtEnd")) return true;
		if (type.equals("comp_op4") && expectedType.equals("comp_op")) return true;
		if (type.equals("AnyName10") && expectedType.equals("AnyName")) return true;
		if (type.equals("try_stmt_End2") && expectedType.equals("try_stmt_End")) return true;
		if (type.equals("termEnd1") && expectedType.equals("termEnd")) return true;
		if (type.equals("AnyName24") && expectedType.equals("AnyName")) return true;
		if (type.equals("Number5") && expectedType.equals("Number")) return true;
		if (type.equals("comp_op11") && expectedType.equals("comp_op")) return true;
		if (type.equals("not_test2") && expectedType.equals("not_test")) return true;
		if (type.equals("classsmall_stmt7") && expectedType.equals("classsmall_stmt")) return true;
		if (type.equals("shift_exprEnd1") && expectedType.equals("shift_exprEnd")) return true;
		if (type.equals("decorator_End2") && expectedType.equals("decorator_End")) return true;
		if (type.equals("expr_stmtEnd12") && expectedType.equals("expr_stmtEnd")) return true;
		if (type.equals("atomtrailerEnd2") && expectedType.equals("atomtrailerEnd")) return true;
		if (type.equals("atom8") && expectedType.equals("atom")) return true;
		if (type.equals("arglist1") && expectedType.equals("arglist")) return true;
		if (type.equals("StringNode4") && expectedType.equals("StringNode")) return true;
		if (type.equals("flow_stmt2") && expectedType.equals("flow_stmt")) return true;
		if (type.equals("comp_op3") && expectedType.equals("comp_op")) return true;
		if (type.equals("AnyName11") && expectedType.equals("AnyName")) return true;
		if (type.equals("listmakerEnd1") && expectedType.equals("listmakerEnd")) return true;
		if (type.equals("print_stmt3") && expectedType.equals("print_stmt")) return true;
		if (type.equals("Number4") && expectedType.equals("Number")) return true;
		if (type.equals("small_stmt8") && expectedType.equals("small_stmt")) return true;
		if (type.equals("expr_stmtEnd5") && expectedType.equals("expr_stmtEnd")) return true;
		if (type.equals("AnyName21") && expectedType.equals("AnyName")) return true;
		if (type.equals("subscript3") && expectedType.equals("subscript")) return true;
		if (type.equals("import_stmt2") && expectedType.equals("import_stmt")) return true;
		if (type.equals("AnyName6") && expectedType.equals("AnyName")) return true;
		if (type.equals("small_stmt3") && expectedType.equals("small_stmt")) return true;
		if (type.equals("termEnd2") && expectedType.equals("termEnd")) return true;
		if (type.equals("funcdef_End2") && expectedType.equals("funcdef_End")) return true;
		if (type.equals("AnyName29") && expectedType.equals("AnyName")) return true;
		if (type.equals("AnyName8") && expectedType.equals("AnyName")) return true;
		if (type.equals("classsmall_stmt6") && expectedType.equals("classsmall_stmt")) return true;
		if (type.equals("comp_op10") && expectedType.equals("comp_op")) return true;
		if (type.equals("factor4") && expectedType.equals("factor")) return true;
		if (type.equals("ClassDefinition") && expectedType.equals("stmt")) return true;
		if (type.equals("AnyName31") && expectedType.equals("AnyName")) return true;
		if (type.equals("atom9") && expectedType.equals("atom")) return true;
		if (type.equals("atomtrailerEnd3") && expectedType.equals("atomtrailerEnd")) return true;
		if (type.equals("expr_stmtEnd11") && expectedType.equals("expr_stmtEnd")) return true;
		if (type.equals("shift_exprEnd2") && expectedType.equals("shift_exprEnd")) return true;
		if (type.equals("StringNode3") && expectedType.equals("StringNode")) return true;
		if (type.equals("decorator_End_Par2") && expectedType.equals("decorator_End_Par")) return true;
		if (type.equals("AnyName19") && expectedType.equals("AnyName")) return true;
		if (type.equals("flow_stmt3") && expectedType.equals("flow_stmt")) return true;
		if (type.equals("comp_op1") && expectedType.equals("comp_op")) return true;
		if (type.equals("else_stmt_End2") && expectedType.equals("else_stmt_End")) return true;
		if (type.equals("AnyName12") && expectedType.equals("AnyName")) return true;
		if (type.equals("comp_op2") && expectedType.equals("comp_op")) return true;
		if (type.equals("expr_stmtEnd6") && expectedType.equals("expr_stmtEnd")) return true;
		if (type.equals("stmt6") && expectedType.equals("stmt")) return true;
		if (type.equals("Number3") && expectedType.equals("Number")) return true;
		if (type.equals("factor3") && expectedType.equals("factor")) return true;
		if (type.equals("AnyName22") && expectedType.equals("AnyName")) return true;
		if (type.equals("termEnd3") && expectedType.equals("termEnd")) return true;
		if (type.equals("AnyName7") && expectedType.equals("AnyName")) return true;
		if (type.equals("listmakerEnd2") && expectedType.equals("listmakerEnd")) return true;
		if (type.equals("subscript2") && expectedType.equals("subscript")) return true;
		if (type.equals("import_stmt1") && expectedType.equals("import_stmt")) return true;
		if (type.equals("power2") && expectedType.equals("power")) return true;
		if (type.equals("small_stmt2") && expectedType.equals("small_stmt")) return true;
		if (type.equals("small_stmt9") && expectedType.equals("small_stmt")) return true;
		if (type.equals("subscript1") && expectedType.equals("subscript")) return true;
		if (type.equals("Name1") && expectedType.equals("Name")) return true;
		if (type.equals("small_stmt10") && expectedType.equals("small_stmt")) return true;
		if (type.equals("factor1") && expectedType.equals("factor")) return true;
		if (type.equals("StringNode2") && expectedType.equals("StringNode")) return true;
		if (type.equals("atom6") && expectedType.equals("atom")) return true;
		if (type.equals("fpdef2") && expectedType.equals("fpdef")) return true;
		if (type.equals("classsmall_stmt11") && expectedType.equals("classsmall_stmt")) return true;
		if (type.equals("stmt5") && expectedType.equals("stmt")) return true;
		if (type.equals("decorator_End_Par1") && expectedType.equals("decorator_End_Par")) return true;
		if (type.equals("AnyName14") && expectedType.equals("AnyName")) return true;
		if (type.equals("small_stmt5") && expectedType.equals("small_stmt")) return true;
		if (type.equals("AnyName28") && expectedType.equals("AnyName")) return true;
		if (type.equals("print_stmt1") && expectedType.equals("print_stmt")) return true;
		if (type.equals("AnyName4") && expectedType.equals("AnyName")) return true;
		if (type.equals("ImportFromEnd2") && expectedType.equals("ImportFromEnd")) return true;
		if (type.equals("flow_stmt4") && expectedType.equals("flow_stmt")) return true;
		if (type.equals("if_stmt_End2") && expectedType.equals("if_stmt_End")) return true;
		if (type.equals("ImportFrom1") && expectedType.equals("ImportFrom")) return true;
		if (type.equals("atomtrailerEnd4") && expectedType.equals("atomtrailerEnd")) return true;
		if (type.equals("expr_stmtEnd3") && expectedType.equals("expr_stmtEnd")) return true;
		if (type.equals("FunctionDefinition") && expectedType.equals("stmt")) return true;
		if (type.equals("termEnd4") && expectedType.equals("termEnd")) return true;
		if (type.equals("for_stmt_End2") && expectedType.equals("for_stmt_End")) return true;
		if (type.equals("Number2") && expectedType.equals("Number")) return true;
		if (type.equals("power1") && expectedType.equals("power")) return true;
		if (type.equals("classsmall_stmt2") && expectedType.equals("classsmall_stmt")) return true;
		if (type.equals("finally_stmt_End1") && expectedType.equals("finally_stmt_End")) return true;
		if (type.equals("comp_op9") && expectedType.equals("comp_op")) return true;
		if (type.equals("arith_exprEnd2") && expectedType.equals("arith_exprEnd")) return true;
		if (type.equals("atom5") && expectedType.equals("atom")) return true;
		if (type.equals("AnyName27") && expectedType.equals("AnyName")) return true;
		if (type.equals("factor2") && expectedType.equals("factor")) return true;
		if (type.equals("Name2") && expectedType.equals("Name")) return true;
		if (type.equals("StringNode1") && expectedType.equals("StringNode")) return true;
		if (type.equals("expr_stmtEnd13") && expectedType.equals("expr_stmtEnd")) return true;
		if (type.equals("atom7") && expectedType.equals("atom")) return true;
		if (type.equals("stmt4") && expectedType.equals("stmt")) return true;
		if (type.equals("classsmall_stmt10") && expectedType.equals("classsmall_stmt")) return true;
		if (type.equals("classsmall_stmt8") && expectedType.equals("classsmall_stmt")) return true;
		if (type.equals("print_stmt2") && expectedType.equals("print_stmt")) return true;
		if (type.equals("AnyName13") && expectedType.equals("AnyName")) return true;
		if (type.equals("small_stmt4") && expectedType.equals("small_stmt")) return true;
		if (type.equals("flow_stmt5") && expectedType.equals("flow_stmt")) return true;
		if (type.equals("if_stmt_End1") && expectedType.equals("if_stmt_End")) return true;
		if (type.equals("AnyName5") && expectedType.equals("AnyName")) return true;
		if (type.equals("Number1") && expectedType.equals("Number")) return true;
		if (type.equals("ImportFrom2") && expectedType.equals("ImportFrom")) return true;
		if (type.equals("AnyName20") && expectedType.equals("AnyName")) return true;
		if (type.equals("classsmall_stmt9") && expectedType.equals("classsmall_stmt")) return true;
		if (type.equals("expr_stmtEnd4") && expectedType.equals("expr_stmtEnd")) return true;
		if (type.equals("classsmall_stmt3") && expectedType.equals("classsmall_stmt")) return true;
		if (type.equals("comp_op8") && expectedType.equals("comp_op")) return true;
		if (type.equals("stmt3") && expectedType.equals("stmt")) return true;
		if (type.equals("AnyName26") && expectedType.equals("AnyName")) return true;
		if (type.equals("AnyName16") && expectedType.equals("AnyName")) return true;
		if (type.equals("classsmall_stmt13") && expectedType.equals("classsmall_stmt")) return true;
		if (type.equals("small_stmt7") && expectedType.equals("small_stmt")) return true;
		if (type.equals("test1") && expectedType.equals("test")) return true;
		if (type.equals("expr_stmtEnd9") && expectedType.equals("expr_stmtEnd")) return true;
		if (type.equals("except_clause_End2") && expectedType.equals("except_clause_End")) return true;
		if (type.equals("elif_stmt_End1") && expectedType.equals("elif_stmt_End")) return true;
		if (type.equals("AnyName2") && expectedType.equals("AnyName")) return true;
		if (type.equals("except_clause_End1") && expectedType.equals("except_clause_End")) return true;
		if (type.equals("expr_stmtEnd1") && expectedType.equals("expr_stmtEnd")) return true;
		if (type.equals("try_stmt_End1") && expectedType.equals("try_stmt_End")) return true;
		if (type.equals("comp_op7") && expectedType.equals("comp_op")) return true;
		if (type.equals("atom3") && expectedType.equals("atom")) return true;
		if (type.equals("stmt2") && expectedType.equals("stmt")) return true;
		if (type.equals("AnyName25") && expectedType.equals("AnyName")) return true;
		if (type.equals("AnyName15") && expectedType.equals("AnyName")) return true;
		if (type.equals("classsmall_stmt12") && expectedType.equals("classsmall_stmt")) return true;
		if (type.equals("test2") && expectedType.equals("test")) return true;
		if (type.equals("StringNode8") && expectedType.equals("StringNode")) return true;
		if (type.equals("small_stmt6") && expectedType.equals("small_stmt")) return true;
		if (type.equals("elif_stmt_End2") && expectedType.equals("elif_stmt_End")) return true;
		if (type.equals("AnyName3") && expectedType.equals("AnyName")) return true;
		if (type.equals("classsmall_stmt1") && expectedType.equals("classsmall_stmt")) return true;
		if (type.equals("except_clause_test_End2") && expectedType.equals("except_clause_test_End")) return true;
		if (type.equals("for_stmt_End1") && expectedType.equals("for_stmt_End")) return true;
		if (type.equals("expr_stmtEnd2") && expectedType.equals("expr_stmtEnd")) return true;
		if (type.equals("StringNode7") && expectedType.equals("StringNode")) return true;
		if (type.equals("classdef_End1") && expectedType.equals("classdef_End")) return true;
		if (type.equals("atom4") && expectedType.equals("atom")) return true;
		if (type.equals("finally_stmt_End2") && expectedType.equals("finally_stmt_End")) return true;
		if (type.equals("comp_op6") && expectedType.equals("comp_op")) return true;
		if (type.equals("arith_exprEnd1") && expectedType.equals("arith_exprEnd")) return true;
		return false;
	}
}
