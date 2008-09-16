package modification.content;

import java.io.File;
import java.io.StringReader;

import cide.gparser.OffsetCharStream;

import tmp.generated_java15.Java15Parser;

import modification.JavaMethodBodyOverrideModification;
import modification.content.Parseables.CSharp.CSharpFile;
import modification.content.Parseables.CSharp.CSharpMethod;
import modification.content.Parseables.java.JavaMethod;
import modification.content.Parseables.java.JavaMethodBody;

public class ParseableTest {

    /**
     * @param args
     * @throws Exception
     * 
     */
    public static void main(String[] args) throws Exception {
	// System.out.println(ContentGenerator.createContent("c.function",
	// "int main( void ) { printf( \"Hallo\n\" ); }").getFST());
	// System.out.println(ContentGenerator.createContent("java.method",
	// "void test() {a = 0; foo(); }").getFST());
	// System.out.println(FSTGenerator.createContent("haskell.definition",
	// "").getFST());

	// System.out
	// .println(ContentGenerator
	// .createContent(
	// new File(
	// "examples/Modification/ConstructedSimpleTests/ClassicFeature/Edge.java"
	// ))
	// .getFST());
	// System.out.println(ContentGenerator.createContent(
	// new File("examples/Tests/C1/Test.c")).getFST());
	// System.out.println(ContentGenerator.createContent(
	// new File("examples/ArithHaskellFull/Arith.hs")).getFST());

	// System.out.println(new CSharpFile(new File(
	// "examples/GPLCSharp/Number/Graph.cs")).getFST());
	//
	// System.out
	// .println(new CSharpMethod(
	// "public void NumberVertices(){GraphSearch(new NumberWorkSpace());}")
	// .getFST());

	

	System.out.println(new JavaMethodBody("{System.out.println();}")
		.getFST());
    }
}
