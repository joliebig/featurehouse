import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import org.junit.Test;

import cide.astgen.nparser.parser.ParseException;
import cide.gparser.CharStream;
import cide.gparser.OffsetCharStream;
import de.ovgu.cide.fstgen.parsers.generated_sdf.SDFParser;

public class SDFParserTest {

	private CharStream loadFile(String fname) throws FileNotFoundException {
		return new OffsetCharStream(new FileInputStream(new File(fname)));
	}

	private void parseFile(CharStream input) throws ParseException,
			cide.gparser.ParseException {
		SDFParser p = new SDFParser(input);
		p.Module(false);
	}

	@Test
	public void testFile_common() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/Common.sdf"));
	}

	@Test
	public void testFile_javascript() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/Javascript.sdf"));
	}

	@Test
	public void testFile_js_common() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/js-common.sdf"));
	}

	@Test
	public void testFile_action() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/MoBL-Action.sdf"));
	}

	@Test
	public void testFile_config() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/MoBL-Config.sdf"));
	}

	@Test
	public void testFile_data() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/MoBL-Data.sdf"));
	}

	@Test
	public void testFile_external() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/MoBL-External.sdf"));
	}

	@Test
	public void testFile_regex() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/MoBL-Regex.sdf"));
	}

	@Test
	public void testFile_Server() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/MoBL-Server.sdf"));
	}

	@Test
	public void testFile_Service() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/MoBL-Service.sdf"));
	}

	@Test
	public void testFile_string() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/MoBL-String.sdf"));
	}

	@Test
	public void testFile_style() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/MoBL-Style.sdf"));
	}

	@Test
	public void testFile_type() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/MoBL-Type.sdf"));
	}

	@Test
	public void testFile_ui() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/MoBL-UI.sdf"));
	}

	@Test
	public void testFile_mobl() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/MoBL.sdf"));
	}

	@Test
	public void testFile_stratego_javascript() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/Stratego-Javascript.sdf"));
	}

	@Test
	public void testFile_stratego_mobl() throws FileNotFoundException,
			cide.gparser.ParseException, ParseException {
		parseFile(loadFile("test/mobl_sdf_testfiles/Stratego-MoBL.sdf"));
	}
}
