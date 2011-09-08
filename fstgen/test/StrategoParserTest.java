import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.File;
import org.junit.Test;

import tmp.generated_stratego.StrategoParser;

import cide.astgen.nparser.parser.ParseException;
import cide.gparser.CharStream;
import cide.gparser.OffsetCharStream;


public class StrategoParserTest {

	private CharStream loadFile(String fname) throws FileNotFoundException {
		return new OffsetCharStream(new FileInputStream(new File(fname)));
	}

	private void parseFile(CharStream input) throws ParseException, cide.gparser.ParseException {
		StrategoParser p = new StrategoParser(input);
		p.Module(false);
	}

	@Test
	public void testFile_arity_test() throws FileNotFoundException, cide.gparser.ParseException, ParseException {
		parseFile(loadFile("/work/joliebig/workspace_sple/featurehouse/fstgen/test/str_testdateien/arity_test.str"));
	}

}
