package tmp.generated_scheme2;

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
		throw new RuntimeException("Unknown Non Terminal in FST "+nonTerminal);
	}
	protected boolean isSubtype(String type, String expectedType) {
		if (type.equals(expectedType)) return true;
		if (type.equals("definition2") && expectedType.equals("definition")) return true;
		if (type.equals("expression6") && expectedType.equals("expression")) return true;
		if (type.equals("derived_expression3") && expectedType.equals("derived_expression")) return true;
		if (type.equals("alternate1") && expectedType.equals("alternate")) return true;
		if (type.equals("self_evaluating2") && expectedType.equals("self_evaluating")) return true;
		if (type.equals("abbrev_prefix2") && expectedType.equals("abbrev_prefix")) return true;
		if (type.equals("simple_datum1") && expectedType.equals("simple_datum")) return true;
		if (type.equals("literal2") && expectedType.equals("literal")) return true;
		if (type.equals("formals1") && expectedType.equals("formals")) return true;
		if (type.equals("command_or_definition2") && expectedType.equals("command_or_definition")) return true;
		if (type.equals("cond_end1") && expectedType.equals("cond_end")) return true;
		if (type.equals("derived_expression7") && expectedType.equals("derived_expression")) return true;
		if (type.equals("quotation1") && expectedType.equals("quotation")) return true;
		if (type.equals("definition1") && expectedType.equals("definition")) return true;
		if (type.equals("definition3") && expectedType.equals("definition")) return true;
		if (type.equals("derived_expression12") && expectedType.equals("derived_expression")) return true;
		if (type.equals("expression5") && expectedType.equals("expression")) return true;
		if (type.equals("derived_expression2") && expectedType.equals("derived_expression")) return true;
		if (type.equals("simple_datum2") && expectedType.equals("simple_datum")) return true;
		if (type.equals("formals2") && expectedType.equals("formals")) return true;
		if (type.equals("self_evaluating1") && expectedType.equals("self_evaluating")) return true;
		if (type.equals("abbrev_prefix3") && expectedType.equals("abbrev_prefix")) return true;
		if (type.equals("literal1") && expectedType.equals("literal")) return true;
		if (type.equals("simple_datum5") && expectedType.equals("simple_datum")) return true;
		if (type.equals("list3") && expectedType.equals("list")) return true;
		if (type.equals("derived_expression6") && expectedType.equals("derived_expression")) return true;
		if (type.equals("case_end1") && expectedType.equals("case_end")) return true;
		if (type.equals("derived_expression1") && expectedType.equals("derived_expression")) return true;
		if (type.equals("expression4") && expectedType.equals("expression")) return true;
		if (type.equals("expression2") && expectedType.equals("expression")) return true;
		if (type.equals("datum1") && expectedType.equals("datum")) return true;
		if (type.equals("formals3") && expectedType.equals("formals")) return true;
		if (type.equals("compound_datum1") && expectedType.equals("compound_datum")) return true;
		if (type.equals("abbrev_prefix4") && expectedType.equals("abbrev_prefix")) return true;
		if (type.equals("case_end2") && expectedType.equals("case_end")) return true;
		if (type.equals("simple_datum4") && expectedType.equals("simple_datum")) return true;
		if (type.equals("derived_expression9") && expectedType.equals("derived_expression")) return true;
		if (type.equals("derived_expression11") && expectedType.equals("derived_expression")) return true;
		if (type.equals("self_evaluating3") && expectedType.equals("self_evaluating")) return true;
		if (type.equals("derived_expression5") && expectedType.equals("derived_expression")) return true;
		if (type.equals("list1") && expectedType.equals("list")) return true;
		if (type.equals("alternate2") && expectedType.equals("alternate")) return true;
		if (type.equals("expression3") && expectedType.equals("expression")) return true;
		if (type.equals("expression1") && expectedType.equals("expression")) return true;
		if (type.equals("command_or_definition1") && expectedType.equals("command_or_definition")) return true;
		if (type.equals("datum2") && expectedType.equals("datum")) return true;
		if (type.equals("abbrev_prefix1") && expectedType.equals("abbrev_prefix")) return true;
		if (type.equals("expression7") && expectedType.equals("expression")) return true;
		if (type.equals("compound_datum2") && expectedType.equals("compound_datum")) return true;
		if (type.equals("simple_datum3") && expectedType.equals("simple_datum")) return true;
		if (type.equals("derived_expression8") && expectedType.equals("derived_expression")) return true;
		if (type.equals("quotation2") && expectedType.equals("quotation")) return true;
		if (type.equals("list2") && expectedType.equals("list")) return true;
		if (type.equals("self_evaluating4") && expectedType.equals("self_evaluating")) return true;
		if (type.equals("derived_expression4") && expectedType.equals("derived_expression")) return true;
		if (type.equals("cond_end2") && expectedType.equals("cond_end")) return true;
		if (type.equals("derived_expression10") && expectedType.equals("derived_expression")) return true;
		return false;
	}
}
