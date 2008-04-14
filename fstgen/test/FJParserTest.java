import java.io.FileInputStream;
import java.io.FileNotFoundException;
import tmp.generated_fj.*;

import org.junit.Test;

import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class FJParserTest {
	@Test
	public void runParser() throws FileNotFoundException, ParseException {
		FJParser p = new FJParser(new OffsetCharStream( new FileInputStream("test/Complex.fj")));
		p.TypeDeclaration(false);
		System.out.println(p.getRoot().printFST(0));
	}
}
