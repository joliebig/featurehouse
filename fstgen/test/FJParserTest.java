import java.io.FileInputStream;
import java.io.FileNotFoundException;

import org.junit.Test;

import tmp.generated_fj.FJParser;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;

public class FJParserTest {
	@Test
	public void runParser() throws FileNotFoundException, ParseException {
		FJParser p = new FJParser(new OffsetCharStream( new FileInputStream("test/Complex.fj")));
		p.TypeDeclaration(false);
		System.out.println(p.getRoot().printFST(0));
	}
}
