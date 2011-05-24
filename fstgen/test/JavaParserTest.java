import java.io.FileInputStream;
import java.io.FileNotFoundException;

import org.junit.Test;

import tmp.generated_java15.Java15Parser;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;

public class JavaParserTest {
	@Test
	public void runParser() throws FileNotFoundException, ParseException {
		Java15Parser p = new Java15Parser(new OffsetCharStream( new FileInputStream("test/Test.java")));
		p.CompilationUnit(false);
		System.out.println(p.getRoot().printFST(0));
	}
}
