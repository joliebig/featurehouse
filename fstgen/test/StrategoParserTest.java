import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import org.junit.Test;

import tmp.generated_stratego.StrategoParser;

import cide.astgen.nparser.parser.ParseException;
import cide.gparser.CharStream;
import cide.gparser.OffsetCharStream;

public class StrategoParserTest {

	private CharStream loadFile(String fname) throws FileNotFoundException {
		return new OffsetCharStream(new FileInputStream(new File(fname)));
	}

	private void parseFile(CharStream input) throws ParseException,
			cide.gparser.ParseException {
		StrategoParser p = new StrategoParser(input);
		p.Module(false);
	}

	@Test
	public void testFile_action() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/action.str"));
	}

	@Test
	public void testFile_analyze() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/analyze.str"));
	}

	@Test
	public void testFile_check() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/check.str"));
	}

	@Test
	public void testFile_compile() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/compile.str"));
	}

	@Test
	public void testFile_complete() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/complete.str"));
	}

	@Test
	public void testFile_cps_action() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/cps-action.str"));
	}

	@Test
	public void testFile_cps_lift() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/cps-lift.str"));
	}

	@Test
	public void testFile_css() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/css.str"));
	}

	@Test
	public void testFile_data() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/data.str"));
	}

	@Test
	public void testFile_dead_code() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/dead-code.str"));
	}

	@Test
	public void testFile_declare() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/declare.str"));
	}

	@Test
	public void testFile_desugar() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/desugar.str"));
	}

	@Test
	public void testFile_editor_resolve() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/editor_resolve.str"));
	}

	@Test
	public void testFile_editor() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/editor.str"));
	}

	@Test
	public void testFile_emit() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/emit.str"));
	}

	@Test
	public void testFile_generation_type() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/generation_type.str"));
	}

	@Test
	public void testFile_generation_util() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/generation_util.str"));
	}

	@Test
	public void testFile_hover() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/hover.str"));
	}

	@Test
	public void testFile_html() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/html.str"));
	}

	@Test
	public void testFile_lookup() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/lookup.str"));
	}

	@Test
	public void testFile_mobl() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/mobl.str"));
	}

	@Test
	public void testFile_moblc() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/moblc.str"));
	}

	@Test
	public void testFile_normalize() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/normalize.str"));
	}

	@Test
	public void testFile_optimize() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/optimize.str"));
	}

	@Test
	public void testFile_pp() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/pp.str"));
	}

	@Test
	public void testFile_rename() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/rename.str"));
	}

	@Test
	public void testFile_resolve() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/resolve.str"));
	}

	@Test
	public void testFile_script() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/script.str"));
	}

	@Test
	public void testFile_server() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/server.str"));
	}

	@Test
	public void testFile_service() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/service.str"));
	}

	@Test
	public void testFile_sql() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/sql.str"));
	}

	@Test
	public void testFile_style() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/style.str"));
	}

	@Test
	public void testFile_sync_analysis() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/sync-analysis.str"));
	}

	@Test
	public void testFile_type() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/type.str"));
	}

	@Test
	public void testFile_ui_lift() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/ui-lift.str"));
	}

	@Test
	public void testFile_ui() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/ui.str"));
	}

	@Test
	public void testFile_util() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_stratego_testfiles/util.str"));
	}

	@Test
	public void testFile_exprlang_check() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/exprlang_testdateien/check.str"));
	}

	@Test
	public void testFile_exprlang_eval() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/exprlang_testdateien/eval.str"));
	}

	@Test
	public void testFile_exprlang() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/exprlang_testdateien/exprlang.str"));
	}

	@Test
	public void testFile_exprlang_generate() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/exprlang_testdateien/generate.str"));
	}

	@Test
	public void testFile_exprlang_modifyAst() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/exprlang_testdateien/modifyAst.str"));
	}

	@Test
	public void testFile_exprlang_simplify() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/exprlang_testdateien/simplify.str"));
	}

	@Test
	public void testFile_arity_test() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/arity_test.str"));
	}

	@Test
	public void testFile_as_pattern() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/as_pattern.str"));
	}

	@Test
	public void testFile_assignment() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/assignment.str"));
	}

	@Test
	public void testFile_comment_test() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/comment_test.str"));
	}

	@Test
	public void testFile_conditional_innermost() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/conditional_innermost.str"));
	}

	@Test
	public void testFile_congruence_arguments() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/congruence_arguments.str"));
	}

	@Test
	public void testFile_congruence_thread() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/congruence_thread.str"));
	}

	@Test
	public void testFile_cs_test01() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/cs_test01.str"));
	}

	@Test
	public void testFile_cs_test02() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/cs_test02.str"));
	}

	@Test
	public void testFile_cs_test03() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/cs_test03.str"));
	}

	@Test
	public void testFile_cs_test04() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/cs_test04.str"));
	}

	@Test
	public void testFile_dynamic_rules_test() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/dynamic_rules_test.str"));
	}

	@Test
	public void testFile_empty_sections() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/empty_sections.str"));
	}

	@Test
	public void testFile_Expressions() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/Expressions.str"));
	}

	@Test
	public void testFile_flatten_test() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/flatten_test.str"));
	}

	@Test
	public void testFile_higher_order_arg() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/higher_order_arg.str"));
	}

	@Test
	public void testFile_implicit_var_decl() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/implicit_var_decl.str"));
	}

	@Test
	public void testFile_improper_list_test() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/improper_list_test.str"));
	}

	@Test
	public void testFile_innermost_pure() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/innermost_pure.str"));
	}

	@Test
	public void testFile_io_test10() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/io_test10.str"));
	}

	@Test
	public void testFile_io_test11() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/io_test11.str"));
	}

	@Test
	public void testFile_io_test1() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/io_test1.str"));
	}

	@Test
	public void testFile_io_test2() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/io_test2.str"));
	}

	@Test
	public void testFile_io_test3() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/io_test3.str"));
	}

	@Test
	public void testFile_io_test4() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/io_test4.str"));
	}

	@Test
	public void testFile_jtree_parenthesize() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/jtree_parenthesize.str"));
	}

	@Test
	public void testFile_let_test() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/let_test.str"));
	}

	@Test
	public void testFile_match_arity() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/match_arity.str"));
	}

	@Test
	public void testFile_match_automaton_test() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/match_automaton_test.str"));
	}

	@Test
	public void testFile_mkterm() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/mkterm.str"));
	}

	@Test
	public void testFile_nested_defs01() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/nested_defs01.str"));
	}

	@Test
	public void testFile_number_syntax_test() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/number_syntax_test.str"));
	}

	@Test
	public void testFile_number_test() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/number_test.str"));
	}

	@Test
	public void testFile_occan() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/occan.str"));
	}

	@Test
	public void testFile_overlay_test_b() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/overlay_test_b.str"));
	}

	@Test
	public void testFile_overlay_test() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/overlay_test.str"));
	}

	@Test
	public void testFile_overloading_constructors_test()
			throws FileNotFoundException, cide.gparser.ParseException,
			ParseException {
		parseFile(loadFile("test/str_testdateien/overloading_constructors_test.str"));
	}

	@Test
	public void testFile_overloading() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/overloading.str"));
	}

	@Test
	public void testFile_prop() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/prop.str"));
	}

	@Test
	public void testFile_qcons_test01() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/qcons_test01.str"));
	}

	@Test
	public void testFile_qcons_test02() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/qcons_test02.str"));
	}

	@Test
	public void testFile_qcons_test03() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/qcons_test03.str"));
	}

	@Test
	public void testFile_qcons_test04() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/qcons_test04.str"));
	}

	@Test
	public void testFile_rename_test() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/rename_test.str"));
	}

	@Test
	public void testFile_spec() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/spec.str"));
	}

	@Test
	public void testFile_tail() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/tail.str"));
	}

	@Test
	public void testFile_test01() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test01.str"));
	}

	@Test
	public void testFile_test02() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test02.str"));
	}

	@Test
	public void testFile_test03() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test03.str"));
	}

	@Test
	public void testFile_test04() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test04.str"));
	}

	@Test
	public void testFile_test05() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test05.str"));
	}

	@Test
	public void testFile_test06() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test06.str"));
	}

	@Test
	public void testFile_test07() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test07.str"));
	}

	@Test
	public void testFile_test08() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test08.str"));
	}

	@Test
	public void testFile_test09() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test09.str"));
	}

	@Test
	public void testFile_test100() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test100.str"));
	}

	@Test
	public void testFile_test101() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test101.str"));
	}

	@Test
	public void testFile_test102() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test102.str"));
	}

	@Test
	public void testFile_test103() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test103.str"));
	}

	@Test
	public void testFile_test104() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test104.str"));
	}

	@Test
	public void testFile_test105() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test105.str"));
	}

	@Test
	public void testFile_test106() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test106.str"));
	}

	@Test
	public void testFile_test107() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test107.str"));
	}

	@Test
	public void testFile_test108() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test108.str"));
	}

	@Test
	public void testFile_test109() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test109.str"));
	}

	@Test
	public void testFile_test10() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test10.str"));
	}

	@Test
	public void testFile_test110() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test110.str"));
	}

	@Test
	public void testFile_test111() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test111.str"));
	}

	@Test
	public void testFile_test112() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test112.str"));
	}

	@Test
	public void testFile_test113() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test113.str"));
	}

	@Test
	public void testFile_test114() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test114.str"));
	}

	@Test
	public void testFile_test115() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test115.str"));
	}

	@Test
	public void testFile_test11() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test11.str"));
	}

	@Test
	public void testFile_test12() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test12.str"));
	}

	@Test
	public void testFile_test13() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test13.str"));
	}

	@Test
	public void testFile_test14() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test14.str"));
	}

	@Test
	public void testFile_test15() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test15.str"));
	}

	@Test
	public void testFile_test16() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test16.str"));
	}

	@Test
	public void testFile_test17() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test17.str"));
	}

	@Test
	public void testFile_test18() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test18.str"));
	}

	@Test
	public void testFile_test19() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test19.str"));
	}

	@Test
	public void testFile_test20() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test20.str"));
	}

	@Test
	public void testFile_test21() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test21.str"));
	}

	@Test
	public void testFile_test22() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test22.str"));
	}

	@Test
	public void testFile_test23() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test23.str"));
	}

	@Test
	public void testFile_test24() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test24.str"));
	}

	@Test
	public void testFile_test25() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test25.str"));
	}

	@Test
	public void testFile_test27() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test27.str"));
	}

	@Test
	public void testFile_test28a() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test28a.str"));
	}

	@Test
	public void testFile_test28b() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test28b.str"));
	}

	@Test
	public void testFile_test28() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test28.str"));
	}

	@Test
	public void testFile_test29() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test29.str"));
	}

	@Test
	public void testFile_test30() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test30.str"));
	}

	@Test
	public void testFile_test31() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test31.str"));
	}

	@Test
	public void testFile_test32() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test32.str"));
	}

	@Test
	public void testFile_test33() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test33.str"));
	}

	@Test
	public void testFile_test34() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test34.str"));
	}

	@Test
	public void testFile_test35() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test35.str"));
	}

	@Test
	public void testFile_test36() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test36.str"));
	}

	@Test
	public void testFile_test37() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test37.str"));
	}

	@Test
	public void testFile_test38() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test38.str"));
	}

	@Test
	public void testFile_test39() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test39.str"));
	}

	@Test
	public void testFile_test40() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test40.str"));
	}

	@Test
	public void testFile_test41() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test41.str"));
	}

	@Test
	public void testFile_test43() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test43.str"));
	}

	@Test
	public void testFile_test44() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test44.str"));
	}

	@Test
	public void testFile_test45() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test45.str"));
	}

	@Test
	public void testFile_test46() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test46.str"));
	}

	@Test
	public void testFile_test47() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test47.str"));
	}

	@Test
	public void testFile_test48() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test48.str"));
	}

	@Test
	public void testFile_test49() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test49.str"));
	}

	@Test
	public void testFile_test50() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test50.str"));
	}

	@Test
	public void testFile_test51() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test51.str"));
	}

	@Test
	public void testFile_test53() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test53.str"));
	}

	@Test
	public void testFile_test54() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test54.str"));
	}

	@Test
	public void testFile_test56() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test56.str"));
	}

	@Test
	public void testFile_test57() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test57.str"));
	}

	@Test
	public void testFile_test58() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test58.str"));
	}

	@Test
	public void testFile_test59() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test59.str"));
	}

	@Test
	public void testFile_test60() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test60.str"));
	}

	@Test
	public void testFile_test61() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test61.str"));
	}

	@Test
	public void testFile_test62() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test62.str"));
	}

	@Test
	public void testFile_test63() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test63.str"));
	}

	@Test
	public void testFile_test64() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test64.str"));
	}

	@Test
	public void testFile_test65() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test65.str"));
	}

	@Test
	public void testFile_test67() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test67.str"));
	}

	@Test
	public void testFile_test68() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test68.str"));
	}

	@Test
	public void testFile_test69() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test69.str"));
	}

	@Test
	public void testFile_test70() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test70.str"));
	}

	@Test
	public void testFile_test72() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test72.str"));
	}

	@Test
	public void testFile_test73() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test73.str"));
	}

	@Test
	public void testFile_test74() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test74.str"));
	}

	@Test
	public void testFile_test75() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test75.str"));
	}

	@Test
	public void testFile_test76() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test76.str"));
	}

	@Test
	public void testFile_test77() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test77.str"));
	}

	@Test
	public void testFile_test78() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test78.str"));
	}

	@Test
	public void testFile_test79() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test79.str"));
	}

	@Test
	public void testFile_test80() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test80.str"));
	}

	@Test
	public void testFile_test81() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test81.str"));
	}

	@Test
	public void testFile_test82() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test82.str"));
	}

	@Test
	public void testFile_test83() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test83.str"));
	}

	@Test
	public void testFile_test84() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test84.str"));
	}

	@Test
	public void testFile_test85() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test85.str"));
	}

	@Test
	public void testFile_test86() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test86.str"));
	}

	@Test
	public void testFile_test87() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test87.str"));
	}

	@Test
	public void testFile_test88() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test88.str"));
	}

	@Test
	public void testFile_test89() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test89.str"));
	}

	@Test
	public void testFile_test90() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test90.str"));
	}

	@Test
	public void testFile_test91() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test91.str"));
	}

	@Test
	public void testFile_test92() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test92.str"));
	}

	@Test
	public void testFile_test93() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test93.str"));
	}

	@Test
	public void testFile_test94() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test94.str"));
	}

	@Test
	public void testFile_test95() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test95.str"));
	}

	@Test
	public void testFile_test96() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test96.str"));
	}

	@Test
	public void testFile_test97() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test97.str"));
	}

	@Test
	public void testFile_test98() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test98.str"));
	}

	@Test
	public void testFile_test99() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test99.str"));
	}

	@Test
	public void testFile_test_chain01() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test_chain01.str"));
	}

	@Test
	public void testFile_test_chain02est() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test_chain02.str"));
	}

	@Test
	public void testFile_test_chain03() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test_chain03.str"));
	}

	@Test
	public void testFile_test_libstrc() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/test_libstrc.str"));
	}

	@Test
	public void testFile_thread_test() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/thread_test.str"));
	}

	@Test
	public void testFile_traversal_test() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/traversal_test.str"));
	}

	@Test
	public void testFile_twice() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/twice.str"));
	}

	@Test
	public void testFile_unification_test() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/str_testdateien/unification_test.str"));
	}
}
