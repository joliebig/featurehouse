package modification.content;

import java.io.File;

public class ParseableTest {

    /**
     * @param args
     * @throws Exception
     * 
     */
    public static void main(String[] args) throws Exception {
	 System.out.println(ContentGenerator.createContent("c.function",
	 "int main( void ) { printf( \"Hallo\n\" ); }").getFST());
	System.out.println(ContentGenerator.createContent("java.method",
		"void test() {a = 0; foo(); }").getFST());
	// System.out.println(FSTGenerator.createContent("haskell.definition",
	// "").getFST());

	System.out
		.println(ContentGenerator
			.createContent(
				new File(
					"examples/Modification/ConstructedSimpleTests/ClassicFeature/Edge.java"))
			.getFST());
	System.out.println(ContentGenerator.createContent(
		new File("examples/Tests/C1/Test.c")).getFST());
	System.out.println(ContentGenerator.createContent(
		new File("examples/ArithHaskellFull/Arith.hs")).getFST());
    }
}
