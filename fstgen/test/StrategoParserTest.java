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
		parseFile(loadFile("test/exprlang_testfiles/check.str"));
	}

	@Test
	public void testFile_exprlang_eval() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/exprlang_testfiles/eval.str"));
	}

	@Test
	public void testFile_exprlang() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/exprlang_testfiles/exprlang.str"));
	}

	@Test
	public void testFile_exprlang_generate() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/exprlang_testfiles/generate.str"));
	}

	@Test
	public void testFile_exprlang_modifyAst() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/exprlang_testfiles/modifyAst.str"));
	}

	@Test
	public void testFile_exprlang_simplify() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/exprlang_testfiles/simplify.str"));
	}
}
