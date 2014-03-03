import java.io.FileInputStream;
import java.io.FileNotFoundException;

import org.junit.Test;

import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.parsers.generated_fj.FJParser;

public class FJParserTest {
	@Test
	public void runParser() throws FileNotFoundException, ParseException {
		FJParser p = new FJParser(new OffsetCharStream( new FileInputStream("test/fj_testfiles/Pair.fj")));
		p.TypeDeclaration(false);
		System.out.println(p.getRoot().printFST(0));
	}
}
