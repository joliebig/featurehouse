package de.ovgu.cide.fstgen.parsers.generated_csharp_merge;

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
		if (nonTerminal.getType().equals("compilation_unit")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"using_directive")) {
				v.accept(this);
			}
			{
				FSTNode v=getChild(nonTerminal, "attributes_either");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "compilation_unitEnd");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("compilation_unitEnd")) {
			printFeatures(nonTerminal,true);
			for (FSTNode v : getChildren(nonTerminal,"namespace_member_declaration_no_attr")) {
				v.accept(this);
			}
			for (FSTNode v : getChildren(nonTerminal,"namespace_member_declaration")) {
				v.accept(this);
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("namespace_declaration")) {
			printFeatures(nonTerminal,true);
			printToken("namespace");
			{
				FSTNode v=getChild(nonTerminal, "type_name");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "namespace_body");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("namespace_body")) {
			printFeatures(nonTerminal,true);
			printToken("{");
			hintIncIndent();
			hintNewLine();
			for (FSTNode v : getChildren(nonTerminal,"using_directive")) {
				v.accept(this);
			}
			for (FSTNode v : getChildren(nonTerminal,"namespace_member_declaration")) {
				v.accept(this);
			}
			hintDecIndent();
			hintNewLine();
			printToken("}");
			hintNewLine();
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("namespace_member_declaration_no_attr1")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "namespace_declaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("namespace_member_declaration_no_attr2")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "class_modifiers");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "type_declaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("namespace_member_declaration1")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "namespace_declaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("namespace_member_declaration2")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "attributes");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "class_modifiers");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "namespace_member_declarationEnd");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("namespace_member_declarationEnd1")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "type_declaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("namespace_member_declarationEnd2")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "type");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "typeEnd");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("type_declaration1")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "class_declaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("type_declaration2")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "struct_declaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("type_declaration3")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "interface_declaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("type_declaration4")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "enum_declaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("type_declaration5")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "delegate_declaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("class_declaration")) {
			printFeatures(nonTerminal,true);
			printToken("class");
			{
				FSTNode v=getChild(nonTerminal, "identifier");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "type_parameter_list");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "class_base");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "type_parameter_constraint_clauses");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "class_body");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("class_body")) {
			printFeatures(nonTerminal,true);
			printToken("{");
			hintIncIndent();
			hintNewLine();
			for (FSTNode v : getChildren(nonTerminal,"class_member_declaration")) {
				v.accept(this);
			}
			hintDecIndent();
			hintNewLine();
			printToken("}");
			hintNewLine();
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("class_member_declaration")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "attributes");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "class_modifiers");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "class_member_declarationEnd");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("class_member_declarationEnd5")) {
			printFeatures(nonTerminal,true);
			{
				FSTNode v=getChild(nonTerminal, "type_declaration");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("struct_declaration")) {
			printFeatures(nonTerminal,true);
			printToken("struct");
			{
				FSTNode v=getChild(nonTerminal, "identifier");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "type_parameter_list");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "base_interfaces");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "type_parameter_constraint_clauses");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "class_body");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("base_interfaces")) {
			printFeatures(nonTerminal,true);
			printToken(":");
			{
				FSTNode v=getChild(nonTerminal, "interface_type_list");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("interface_declaration")) {
			printFeatures(nonTerminal,true);
			printToken("interface");
			{
				FSTNode v=getChild(nonTerminal, "identifier");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "type_parameter_list");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "base_interfaces");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "type_parameter_constraint_clauses");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "interface_body");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("interface_body")) {
			printFeatures(nonTerminal,true);
			printToken("{");
			hintIncIndent();
			hintNewLine();
			for (FSTNode v : getChildren(nonTerminal,"interface_member_declaration")) {
				v.accept(this);
			}
			hintDecIndent();
			hintNewLine();
			printToken("}");
			hintNewLine();
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("enum_declaration")) {
			printFeatures(nonTerminal,true);
			printToken("enum");
			{
				FSTNode v=getChild(nonTerminal, "identifier");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "enum_base");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "enum_body");
				if (v!=null) {
					v.accept(this);
				}
			}
			printFeatures(nonTerminal,false);
			return false;
		}
		if (nonTerminal.getType().equals("delegate_declaration")) {
			printFeatures(nonTerminal,true);
			printToken("delegate");
			{
				FSTNode v=getChild(nonTerminal, "type");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "identifier");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "type_parameter_list");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken("(");
			{
				FSTNode v=getChild(nonTerminal, "formal_parameter_list");
				if (v!=null) {
					v.accept(this);
				}
			}
			{
				FSTNode v=getChild(nonTerminal, "type_parameter_constraint_clauses");
				if (v!=null) {
					v.accept(this);
				}
			}
			printToken(")");
			printToken(";");
			hintNewLine();
			printFeatures(nonTerminal,false);
			return false;
		}
		throw new RuntimeException("Unknown Non Terminal in FST "+nonTerminal);
	}
	protected boolean isSubtype(String type, String expectedType) {
		if (type.equals(expectedType)) return true;
		if (type.equals("type_declaration2") && expectedType.equals("type_declaration")) return true;
		if (type.equals("integral_type1") && expectedType.equals("integral_type")) return true;
		if (type.equals("primary_expression_start12") && expectedType.equals("primary_expression_start")) return true;
		if (type.equals("local_variable_initializer3") && expectedType.equals("local_variable_initializer")) return true;
		if (type.equals("class_modifier7") && expectedType.equals("class_modifier")) return true;
		if (type.equals("assignment_operator13") && expectedType.equals("assignment_operator")) return true;
		if (type.equals("embedded_statement12") && expectedType.equals("embedded_statement")) return true;
		if (type.equals("predefined_type2") && expectedType.equals("predefined_type")) return true;
		if (type.equals("IDENTIFIER_CSHARP9") && expectedType.equals("IDENTIFIER_CSHARP")) return true;
		if (type.equals("unary_operator6") && expectedType.equals("unary_operator")) return true;
		if (type.equals("primary_constraint1") && expectedType.equals("primary_constraint")) return true;
		if (type.equals("primary_expression2") && expectedType.equals("primary_expression")) return true;
		if (type.equals("primary_expression_start1") && expectedType.equals("primary_expression_start")) return true;
		if (type.equals("numeric_type3") && expectedType.equals("numeric_type")) return true;
		if (type.equals("assignment_operator2") && expectedType.equals("assignment_operator")) return true;
		if (type.equals("MethodDecl") && expectedType.equals("type_nameEnd")) return true;
		if (type.equals("using_directiveEnd2") && expectedType.equals("using_directiveEnd")) return true;
		if (type.equals("assignment_operator14") && expectedType.equals("assignment_operator")) return true;
		if (type.equals("class_modifier6") && expectedType.equals("class_modifier")) return true;
		if (type.equals("type_declaration3") && expectedType.equals("type_declaration")) return true;
		if (type.equals("integral_type2") && expectedType.equals("integral_type")) return true;
		if (type.equals("try_statement_clauses1") && expectedType.equals("try_statement_clauses")) return true;
		if (type.equals("local_variable_initializer2") && expectedType.equals("local_variable_initializer")) return true;
		if (type.equals("predefined_type1") && expectedType.equals("predefined_type")) return true;
		if (type.equals("embedded_statement13") && expectedType.equals("embedded_statement")) return true;
		if (type.equals("primary_expression_start11") && expectedType.equals("primary_expression_start")) return true;
		if (type.equals("IDENTIFIER_CSHARP8") && expectedType.equals("IDENTIFIER_CSHARP")) return true;
		if (type.equals("unary_operator7") && expectedType.equals("unary_operator")) return true;
		if (type.equals("primary_expression_start2") && expectedType.equals("primary_expression_start")) return true;
		if (type.equals("creation_expressionPostFix1") && expectedType.equals("creation_expressionPostFix")) return true;
		if (type.equals("assignment_operator1") && expectedType.equals("assignment_operator")) return true;
		if (type.equals("using_directiveEnd1") && expectedType.equals("using_directiveEnd")) return true;
		if (type.equals("class_modifier9") && expectedType.equals("class_modifier")) return true;
		if (type.equals("creation_expressionPostFix2") && expectedType.equals("creation_expressionPostFix")) return true;
		if (type.equals("assignment_operator11") && expectedType.equals("assignment_operator")) return true;
		if (type.equals("try_statement_clauses2") && expectedType.equals("try_statement_clauses")) return true;
		if (type.equals("FieldDecl") && expectedType.equals("type_nameEnd")) return true;
		if (type.equals("type_declaration4") && expectedType.equals("type_declaration")) return true;
		if (type.equals("conversion_operator2") && expectedType.equals("conversion_operator")) return true;
		if (type.equals("explicit_anonymous_function_parameter_modifier1") && expectedType.equals("explicit_anonymous_function_parameter_modifier")) return true;
		if (type.equals("embedded_statement14") && expectedType.equals("embedded_statement")) return true;
		if (type.equals("catch_clauseEnd2") && expectedType.equals("catch_clauseEnd")) return true;
		if (type.equals("attribute_target3") && expectedType.equals("attribute_target")) return true;
		if (type.equals("unary_operator8") && expectedType.equals("unary_operator")) return true;
		if (type.equals("namespace_member_declaration_no_attr2") && expectedType.equals("namespace_member_declaration_no_attr")) return true;
		if (type.equals("assignment_operator4") && expectedType.equals("assignment_operator")) return true;
		if (type.equals("assignment_operator12") && expectedType.equals("assignment_operator")) return true;
		if (type.equals("class_modifier8") && expectedType.equals("class_modifier")) return true;
		if (type.equals("unary_expression3") && expectedType.equals("unary_expression")) return true;
		if (type.equals("creation_expressionPostFix3") && expectedType.equals("creation_expressionPostFix")) return true;
		if (type.equals("type_declaration5") && expectedType.equals("type_declaration")) return true;
		if (type.equals("primary_expression_start13") && expectedType.equals("primary_expression_start")) return true;
		if (type.equals("explicit_anonymous_function_parameter_modifier2") && expectedType.equals("explicit_anonymous_function_parameter_modifier")) return true;
		if (type.equals("conversion_operator1") && expectedType.equals("conversion_operator")) return true;
		if (type.equals("catch_clauseEnd1") && expectedType.equals("catch_clauseEnd")) return true;
		if (type.equals("namespace_member_declaration_no_attr1") && expectedType.equals("namespace_member_declaration_no_attr")) return true;
		if (type.equals("assignment_operator3") && expectedType.equals("assignment_operator")) return true;
		if (type.equals("attribute_target_specifier_mod2") && expectedType.equals("attribute_target_specifier_mod")) return true;
		if (type.equals("goto_statementEnd3") && expectedType.equals("goto_statementEnd")) return true;
		if (type.equals("accessor_modifier3") && expectedType.equals("accessor_modifier")) return true;
		if (type.equals("select_or_group_clause2") && expectedType.equals("select_or_group_clause")) return true;
		if (type.equals("body2") && expectedType.equals("body")) return true;
		if (type.equals("integral_type5") && expectedType.equals("integral_type")) return true;
		if (type.equals("class_modifier13") && expectedType.equals("class_modifier")) return true;
		if (type.equals("unary_expression2") && expectedType.equals("unary_expression")) return true;
		if (type.equals("class_modifier14") && expectedType.equals("class_modifier")) return true;
		if (type.equals("primary_expression_start5") && expectedType.equals("primary_expression_start")) return true;
		if (type.equals("event_declarationInt1") && expectedType.equals("event_declarationInt")) return true;
		if (type.equals("overloadable_unary_operator1") && expectedType.equals("overloadable_unary_operator")) return true;
		if (type.equals("global_attribute_target2") && expectedType.equals("global_attribute_target")) return true;
		if (type.equals("interface_member_declarationEnd2") && expectedType.equals("interface_member_declarationEnd")) return true;
		if (type.equals("attribute_target1") && expectedType.equals("attribute_target")) return true;
		if (type.equals("unary_operator2") && expectedType.equals("unary_operator")) return true;
		if (type.equals("integral_type6") && expectedType.equals("integral_type")) return true;
		if (type.equals("goto_statementEnd2") && expectedType.equals("goto_statementEnd")) return true;
		if (type.equals("attribute_target_specifier_mod1") && expectedType.equals("attribute_target_specifier_mod")) return true;
		if (type.equals("overloadable_binary_operator1") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("embedded_statement1") && expectedType.equals("embedded_statement")) return true;
		if (type.equals("accessor_modifier2") && expectedType.equals("accessor_modifier")) return true;
		if (type.equals("selection_statement1") && expectedType.equals("selection_statement")) return true;
		if (type.equals("body1") && expectedType.equals("body")) return true;
		if (type.equals("relational_operator1") && expectedType.equals("relational_operator")) return true;
		if (type.equals("class_modifier12") && expectedType.equals("class_modifier")) return true;
		if (type.equals("unary_expression1") && expectedType.equals("unary_expression")) return true;
		if (type.equals("primary_expression_start6") && expectedType.equals("primary_expression_start")) return true;
		if (type.equals("formal_parameter_listEndInt1") && expectedType.equals("formal_parameter_listEndInt")) return true;
		if (type.equals("identifier3") && expectedType.equals("identifier")) return true;
		if (type.equals("event_declarationInt2") && expectedType.equals("event_declarationInt")) return true;
		if (type.equals("type_name_or_parameter2") && expectedType.equals("type_name_or_parameter")) return true;
		if (type.equals("global_attribute_target1") && expectedType.equals("global_attribute_target")) return true;
		if (type.equals("attribute_target2") && expectedType.equals("attribute_target")) return true;
		if (type.equals("rest_of_array_initializerEnd2") && expectedType.equals("rest_of_array_initializerEnd")) return true;
		if (type.equals("unary_operator3") && expectedType.equals("unary_operator")) return true;
		if (type.equals("selection_statement2") && expectedType.equals("selection_statement")) return true;
		if (type.equals("accessor_modifier1") && expectedType.equals("accessor_modifier")) return true;
		if (type.equals("embedded_statement2") && expectedType.equals("embedded_statement")) return true;
		if (type.equals("boolean_literal2") && expectedType.equals("boolean_literal")) return true;
		if (type.equals("class_modifier11") && expectedType.equals("class_modifier")) return true;
		if (type.equals("integral_type3") && expectedType.equals("integral_type")) return true;
		if (type.equals("local_variable_initializer1") && expectedType.equals("local_variable_initializer")) return true;
		if (type.equals("operator_declaration1") && expectedType.equals("operator_declaration")) return true;
		if (type.equals("identifier2") && expectedType.equals("identifier")) return true;
		if (type.equals("rest_of_array_initializer1") && expectedType.equals("rest_of_array_initializer")) return true;
		if (type.equals("overloadable_unary_operator3") && expectedType.equals("overloadable_unary_operator")) return true;
		if (type.equals("formal_parameter_listEndInt2") && expectedType.equals("formal_parameter_listEndInt")) return true;
		if (type.equals("floating_point_type2") && expectedType.equals("floating_point_type")) return true;
		if (type.equals("indexer_baseInt1") && expectedType.equals("indexer_baseInt")) return true;
		if (type.equals("primary_expression_start3") && expectedType.equals("primary_expression_start")) return true;
		if (type.equals("unary_operator4") && expectedType.equals("unary_operator")) return true;
		if (type.equals("type_declaration1") && expectedType.equals("type_declaration")) return true;
		if (type.equals("predefined_type3") && expectedType.equals("predefined_type")) return true;
		if (type.equals("PropertyDecl") && expectedType.equals("type_nameEnd")) return true;
		if (type.equals("boolean_literal1") && expectedType.equals("boolean_literal")) return true;
		if (type.equals("embedded_statement3") && expectedType.equals("embedded_statement")) return true;
		if (type.equals("class_modifier10") && expectedType.equals("class_modifier")) return true;
		if (type.equals("integral_type4") && expectedType.equals("integral_type")) return true;
		if (type.equals("overloadable_unary_operator2") && expectedType.equals("overloadable_unary_operator")) return true;
		if (type.equals("operator_declaration2") && expectedType.equals("operator_declaration")) return true;
		if (type.equals("rest_of_array_initializer2") && expectedType.equals("rest_of_array_initializer")) return true;
		if (type.equals("ConstructorDecl") && expectedType.equals("typeEnd")) return true;
		if (type.equals("identifier1") && expectedType.equals("identifier")) return true;
		if (type.equals("floating_point_type1") && expectedType.equals("floating_point_type")) return true;
		if (type.equals("indexer_baseInt2") && expectedType.equals("indexer_baseInt")) return true;
		if (type.equals("primary_expression_start4") && expectedType.equals("primary_expression_start")) return true;
		if (type.equals("predefined_type4") && expectedType.equals("predefined_type")) return true;
		if (type.equals("unary_operator5") && expectedType.equals("unary_operator")) return true;
		if (type.equals("class_modifier15") && expectedType.equals("class_modifier")) return true;
		if (type.equals("query_body_clause2") && expectedType.equals("query_body_clause")) return true;
		if (type.equals("overloadable_binary_operator12") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("interface_member_modifier1") && expectedType.equals("interface_member_modifier")) return true;
		if (type.equals("iteration_statement4") && expectedType.equals("iteration_statement")) return true;
		if (type.equals("literal6") && expectedType.equals("literal")) return true;
		if (type.equals("overloadable_unary_operator5") && expectedType.equals("overloadable_unary_operator")) return true;
		if (type.equals("simple_type1") && expectedType.equals("simple_type")) return true;
		if (type.equals("class_member_declarationEnd3") && expectedType.equals("class_member_declarationEnd")) return true;
		if (type.equals("primary_expression_start9") && expectedType.equals("primary_expression_start")) return true;
		if (type.equals("overloadable_binary_operator5") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("relational_operator4") && expectedType.equals("relational_operator")) return true;
		if (type.equals("base_access1") && expectedType.equals("base_access")) return true;
		if (type.equals("yield_statement_body2") && expectedType.equals("yield_statement_body")) return true;
		if (type.equals("embedded_statement4") && expectedType.equals("embedded_statement")) return true;
		if (type.equals("for_initializer1") && expectedType.equals("for_initializer")) return true;
		if (type.equals("integral_type9") && expectedType.equals("integral_type")) return true;
		if (type.equals("namespace_member_declaration2") && expectedType.equals("namespace_member_declaration")) return true;
		if (type.equals("shift_operator1") && expectedType.equals("shift_operator")) return true;
		if (type.equals("overloadable_binary_operator11") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("rest_of_enum_body1") && expectedType.equals("rest_of_enum_body")) return true;
		if (type.equals("query_body_clause3") && expectedType.equals("query_body_clause")) return true;
		if (type.equals("interface_member_modifier2") && expectedType.equals("interface_member_modifier")) return true;
		if (type.equals("literal5") && expectedType.equals("literal")) return true;
		if (type.equals("overloadable_unary_operator4") && expectedType.equals("overloadable_unary_operator")) return true;
		if (type.equals("array_creation_postfix_expressionInternal1") && expectedType.equals("array_creation_postfix_expressionInternal")) return true;
		if (type.equals("simple_type2") && expectedType.equals("simple_type")) return true;
		if (type.equals("class_member_declarationEnd2") && expectedType.equals("class_member_declarationEnd")) return true;
		if (type.equals("base_access2") && expectedType.equals("base_access")) return true;
		if (type.equals("yield_statement_body1") && expectedType.equals("yield_statement_body")) return true;
		if (type.equals("overloadable_binary_operator4") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("IDENTIFIER_CSHARP1") && expectedType.equals("IDENTIFIER_CSHARP")) return true;
		if (type.equals("for_initializer2") && expectedType.equals("for_initializer")) return true;
		if (type.equals("embedded_statement5") && expectedType.equals("embedded_statement")) return true;
		if (type.equals("primary_expression_postfixInternal1") && expectedType.equals("primary_expression_postfixInternal")) return true;
		if (type.equals("rest_of_array_initializerEnd1") && expectedType.equals("rest_of_array_initializerEnd")) return true;
		if (type.equals("iteration_statement2") && expectedType.equals("iteration_statement")) return true;
		if (type.equals("overloadable_binary_operator10") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("type_name_or_parameter1") && expectedType.equals("type_name_or_parameter")) return true;
		if (type.equals("statement1") && expectedType.equals("statement")) return true;
		if (type.equals("rest_of_enum_body2") && expectedType.equals("rest_of_enum_body")) return true;
		if (type.equals("typeEnd2") && expectedType.equals("typeEnd")) return true;
		if (type.equals("class_member_declarationEnd1") && expectedType.equals("class_member_declarationEnd")) return true;
		if (type.equals("class_modifier1") && expectedType.equals("class_modifier")) return true;
		if (type.equals("overloadable_unary_operator7") && expectedType.equals("overloadable_unary_operator")) return true;
		if (type.equals("primary_expression_start7") && expectedType.equals("primary_expression_start")) return true;
		if (type.equals("IDENTIFIER_CSHARP2") && expectedType.equals("IDENTIFIER_CSHARP")) return true;
		if (type.equals("multiplicative_operator3") && expectedType.equals("multiplicative_operator")) return true;
		if (type.equals("relational_operator2") && expectedType.equals("relational_operator")) return true;
		if (type.equals("jump_statement2") && expectedType.equals("jump_statement")) return true;
		if (type.equals("overloadable_binary_operator3") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("formal_parameter_listEnd2") && expectedType.equals("formal_parameter_listEnd")) return true;
		if (type.equals("primary_expression_postfixInternal2") && expectedType.equals("primary_expression_postfixInternal")) return true;
		if (type.equals("secondary_constraints1") && expectedType.equals("secondary_constraints")) return true;
		if (type.equals("embedded_statement6") && expectedType.equals("embedded_statement")) return true;
		if (type.equals("integral_type7") && expectedType.equals("integral_type")) return true;
		if (type.equals("goto_statementEnd1") && expectedType.equals("goto_statementEnd")) return true;
		if (type.equals("resource_acquisition1") && expectedType.equals("resource_acquisition")) return true;
		if (type.equals("iteration_statement3") && expectedType.equals("iteration_statement")) return true;
		if (type.equals("query_body_clause1") && expectedType.equals("query_body_clause")) return true;
		if (type.equals("literal7") && expectedType.equals("literal")) return true;
		if (type.equals("unary_operator1") && expectedType.equals("unary_operator")) return true;
		if (type.equals("statement2") && expectedType.equals("statement")) return true;
		if (type.equals("interface_member_declarationEnd1") && expectedType.equals("interface_member_declarationEnd")) return true;
		if (type.equals("typeEnd3") && expectedType.equals("typeEnd")) return true;
		if (type.equals("primary_expression_start8") && expectedType.equals("primary_expression_start")) return true;
		if (type.equals("overloadable_unary_operator6") && expectedType.equals("overloadable_unary_operator")) return true;
		if (type.equals("IDENTIFIER_CSHARP3") && expectedType.equals("IDENTIFIER_CSHARP")) return true;
		if (type.equals("embedded_statement7") && expectedType.equals("embedded_statement")) return true;
		if (type.equals("relational_operator3") && expectedType.equals("relational_operator")) return true;
		if (type.equals("overloadable_binary_operator2") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("select_or_group_clause1") && expectedType.equals("select_or_group_clause")) return true;
		if (type.equals("primary_expression_postfixInternal3") && expectedType.equals("primary_expression_postfixInternal")) return true;
		if (type.equals("jump_statement1") && expectedType.equals("jump_statement")) return true;
		if (type.equals("integral_type8") && expectedType.equals("integral_type")) return true;
		if (type.equals("namespace_member_declaration1") && expectedType.equals("namespace_member_declaration")) return true;
		if (type.equals("secondary_constraints2") && expectedType.equals("secondary_constraints")) return true;
		if (type.equals("resource_acquisition2") && expectedType.equals("resource_acquisition")) return true;
		if (type.equals("type_parameter_constraints3") && expectedType.equals("type_parameter_constraints")) return true;
		if (type.equals("shift_operator2") && expectedType.equals("shift_operator")) return true;
		if (type.equals("interface_member_declarationEndTypeIdentifier2") && expectedType.equals("interface_member_declarationEndTypeIdentifier")) return true;
		if (type.equals("class_member_declarationEnd7") && expectedType.equals("class_member_declarationEnd")) return true;
		if (type.equals("relational_expressionInternal1") && expectedType.equals("relational_expressionInternal")) return true;
		if (type.equals("overloadable_binary_operator16") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("typeEnd4") && expectedType.equals("typeEnd")) return true;
		if (type.equals("class_type1") && expectedType.equals("class_type")) return true;
		if (type.equals("statement3") && expectedType.equals("statement")) return true;
		if (type.equals("jump_statement4") && expectedType.equals("jump_statement")) return true;
		if (type.equals("literal2") && expectedType.equals("literal")) return true;
		if (type.equals("secondary_constraintsEnd2") && expectedType.equals("secondary_constraintsEnd")) return true;
		if (type.equals("array_creation_postfix_expressionInternal4") && expectedType.equals("array_creation_postfix_expressionInternal")) return true;
		if (type.equals("implicit_anonymous_function_signature1") && expectedType.equals("implicit_anonymous_function_signature")) return true;
		if (type.equals("variable_initializer2") && expectedType.equals("variable_initializer")) return true;
		if (type.equals("assignment_operator8") && expectedType.equals("assignment_operator")) return true;
		if (type.equals("equality_operator1") && expectedType.equals("equality_operator")) return true;
		if (type.equals("primary_expression_postfixInternal4") && expectedType.equals("primary_expression_postfixInternal")) return true;
		if (type.equals("attribute_sectionEnd3") && expectedType.equals("attribute_sectionEnd")) return true;
		if (type.equals("type_parameter_constraints2") && expectedType.equals("type_parameter_constraints")) return true;
		if (type.equals("class_modifier3") && expectedType.equals("class_modifier")) return true;
		if (type.equals("rest_of_enum_bodyEnd1") && expectedType.equals("rest_of_enum_bodyEnd")) return true;
		if (type.equals("overloadable_binary_operator9") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("IDENTIFIER_CSHARP4") && expectedType.equals("IDENTIFIER_CSHARP")) return true;
		if (type.equals("multiplicative_operator1") && expectedType.equals("multiplicative_operator")) return true;
		if (type.equals("embedded_statement8") && expectedType.equals("embedded_statement")) return true;
		if (type.equals("interface_member_declarationEndTypeIdentifier1") && expectedType.equals("interface_member_declarationEndTypeIdentifier")) return true;
		if (type.equals("class_member_declarationEnd6") && expectedType.equals("class_member_declarationEnd")) return true;
		if (type.equals("overloadable_binary_operator15") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("typeEnd5") && expectedType.equals("typeEnd")) return true;
		if (type.equals("class_modifier2") && expectedType.equals("class_modifier")) return true;
		if (type.equals("relational_expressionInternal2") && expectedType.equals("relational_expressionInternal")) return true;
		if (type.equals("jump_statement3") && expectedType.equals("jump_statement")) return true;
		if (type.equals("literal1") && expectedType.equals("literal")) return true;
		if (type.equals("iteration_statement1") && expectedType.equals("iteration_statement")) return true;
		if (type.equals("overloadable_unary_operator8") && expectedType.equals("overloadable_unary_operator")) return true;
		if (type.equals("class_type2") && expectedType.equals("class_type")) return true;
		if (type.equals("array_creation_postfix_expressionInternal5") && expectedType.equals("array_creation_postfix_expressionInternal")) return true;
		if (type.equals("parameter_modifier2") && expectedType.equals("parameter_modifier")) return true;
		if (type.equals("statement4") && expectedType.equals("statement")) return true;
		if (type.equals("parameter_modifier1") && expectedType.equals("parameter_modifier")) return true;
		if (type.equals("variable_initializer1") && expectedType.equals("variable_initializer")) return true;
		if (type.equals("assignment_operator9") && expectedType.equals("assignment_operator")) return true;
		if (type.equals("attribute_sectionEnd2") && expectedType.equals("attribute_sectionEnd")) return true;
		if (type.equals("formal_parameter_listEnd1") && expectedType.equals("formal_parameter_listEnd")) return true;
		if (type.equals("rest_of_enum_bodyEnd2") && expectedType.equals("rest_of_enum_bodyEnd")) return true;
		if (type.equals("argumentPrefix1") && expectedType.equals("argumentPrefix")) return true;
		if (type.equals("type_parameter_constraints1") && expectedType.equals("type_parameter_constraints")) return true;
		if (type.equals("primary_expression_postfixInternal5") && expectedType.equals("primary_expression_postfixInternal")) return true;
		if (type.equals("multiplicative_operator2") && expectedType.equals("multiplicative_operator")) return true;
		if (type.equals("additive_operator1") && expectedType.equals("additive_operator")) return true;
		if (type.equals("assignment_operator10") && expectedType.equals("assignment_operator")) return true;
		if (type.equals("overloadable_binary_operator8") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("IDENTIFIER_CSHARP5") && expectedType.equals("IDENTIFIER_CSHARP")) return true;
		if (type.equals("embedded_statement9") && expectedType.equals("embedded_statement")) return true;
		if (type.equals("overloadable_binary_operator14") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("attribute_section_start1") && expectedType.equals("attribute_section_start")) return true;
		if (type.equals("constructor_initializerInt1") && expectedType.equals("constructor_initializerInt")) return true;
		if (type.equals("non_array_type1") && expectedType.equals("non_array_type")) return true;
		if (type.equals("numeric_type1") && expectedType.equals("numeric_type")) return true;
		if (type.equals("class_member_declarationEnd5") && expectedType.equals("class_member_declarationEnd")) return true;
		if (type.equals("namespace_member_declarationEnd2") && expectedType.equals("namespace_member_declarationEnd")) return true;
		if (type.equals("parameter_modifier3") && expectedType.equals("parameter_modifier")) return true;
		if (type.equals("assignment_operator5") && expectedType.equals("assignment_operator")) return true;
		if (type.equals("literal4") && expectedType.equals("literal")) return true;
		if (type.equals("array_creation_postfix_expressionInternal2") && expectedType.equals("array_creation_postfix_expressionInternal")) return true;
		if (type.equals("relational_operator2I1") && expectedType.equals("relational_operator2I")) return true;
		if (type.equals("class_type3") && expectedType.equals("class_type")) return true;
		if (type.equals("expression2") && expectedType.equals("expression")) return true;
		if (type.equals("primary_constraint3") && expectedType.equals("primary_constraint")) return true;
		if (type.equals("primary_expression_start10") && expectedType.equals("primary_expression_start")) return true;
		if (type.equals("attribute_sectionEnd1") && expectedType.equals("attribute_sectionEnd")) return true;
		if (type.equals("embedded_statement10") && expectedType.equals("embedded_statement")) return true;
		if (type.equals("anonymous_function_body2") && expectedType.equals("anonymous_function_body")) return true;
		if (type.equals("IDENTIFIER_CSHARP6") && expectedType.equals("IDENTIFIER_CSHARP")) return true;
		if (type.equals("additive_operator2") && expectedType.equals("additive_operator")) return true;
		if (type.equals("interface_member_declarationEndType1") && expectedType.equals("interface_member_declarationEndType")) return true;
		if (type.equals("switch_label1") && expectedType.equals("switch_label")) return true;
		if (type.equals("argumentPrefix2") && expectedType.equals("argumentPrefix")) return true;
		if (type.equals("primary_expression_postfixInternal6") && expectedType.equals("primary_expression_postfixInternal")) return true;
		if (type.equals("overloadable_binary_operator7") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("class_modifier5") && expectedType.equals("class_modifier")) return true;
		if (type.equals("overloadable_binary_operator13") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("class_member_declarationEnd4") && expectedType.equals("class_member_declarationEnd")) return true;
		if (type.equals("namespace_member_declarationEnd1") && expectedType.equals("namespace_member_declarationEnd")) return true;
		if (type.equals("primary_expression1") && expectedType.equals("primary_expression")) return true;
		if (type.equals("non_array_type2") && expectedType.equals("non_array_type")) return true;
		if (type.equals("numeric_type2") && expectedType.equals("numeric_type")) return true;
		if (type.equals("attribute_section_start2") && expectedType.equals("attribute_section_start")) return true;
		if (type.equals("constructor_initializerInt2") && expectedType.equals("constructor_initializerInt")) return true;
		if (type.equals("primary_constraint2") && expectedType.equals("primary_constraint")) return true;
		if (type.equals("expression1") && expectedType.equals("expression")) return true;
		if (type.equals("expression3") && expectedType.equals("expression")) return true;
		if (type.equals("secondary_constraintsEnd1") && expectedType.equals("secondary_constraintsEnd")) return true;
		if (type.equals("relational_operator2I2") && expectedType.equals("relational_operator2I")) return true;
		if (type.equals("assignment_operator6") && expectedType.equals("assignment_operator")) return true;
		if (type.equals("array_creation_postfix_expressionInternal3") && expectedType.equals("array_creation_postfix_expressionInternal")) return true;
		if (type.equals("literal3") && expectedType.equals("literal")) return true;
		if (type.equals("jump_statement5") && expectedType.equals("jump_statement")) return true;
		if (type.equals("implicit_anonymous_function_signature2") && expectedType.equals("implicit_anonymous_function_signature")) return true;
		if (type.equals("assignment_operator7") && expectedType.equals("assignment_operator")) return true;
		if (type.equals("IDENTIFIER_CSHARP7") && expectedType.equals("IDENTIFIER_CSHARP")) return true;
		if (type.equals("anonymous_function_body1") && expectedType.equals("anonymous_function_body")) return true;
		if (type.equals("variable_initializer3") && expectedType.equals("variable_initializer")) return true;
		if (type.equals("embedded_statement11") && expectedType.equals("embedded_statement")) return true;
		if (type.equals("interface_member_declarationEndType2") && expectedType.equals("interface_member_declarationEndType")) return true;
		if (type.equals("overloadable_binary_operator6") && expectedType.equals("overloadable_binary_operator")) return true;
		if (type.equals("switch_label2") && expectedType.equals("switch_label")) return true;
		if (type.equals("equality_operator2") && expectedType.equals("equality_operator")) return true;
		if (type.equals("class_modifier4") && expectedType.equals("class_modifier")) return true;
		if (type.equals("argumentPrefix3") && expectedType.equals("argumentPrefix")) return true;
		return false;
	}
}
