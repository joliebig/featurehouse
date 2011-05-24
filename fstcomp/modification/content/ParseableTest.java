package modification.content;

import java.util.LinkedList;
import java.util.List;

import modification.content.Parseables.CSharp.CSharpMethod;

import composer.FSTGenComposer;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

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
	// System.out.println(new JavaMethodBody("{System.out.println();}")
	// .getFST());

	/*
	 * CSHARP SUPERIMPOSITION
	 */
	CSharpMethod csOld = new CSharpMethod("public int method(){nothing();}");
	FSTNode csOldFST = csOld.getFST();
	List<FSTNode> l = new LinkedList<FSTNode>();
	l.add(csOldFST);
	FSTNode featOld = new FSTNonTerminal("Feature", "oldFeature", l);

	CSharpMethod csNew = new CSharpMethod(
		"public int method(){original();something();}");

	FSTNode par = new FSTNonTerminal("nonterminal", "parent");

	System.out.println(FSTGenComposer
		.compose(csNew.getFST(), csOldFST, par).getParent());
	System.out.println();

//	 /*
//	 * JAVA SUPERIMPOSITION
//	 * BUG ARRRRGHHHH
//	 */
//	 JavaMethod jmOld = new
//	 JavaMethod("public int [] method(){nothing();}");
//	 FSTNode jmOldFST = jmOld.getFST();
//	 List<FSTNode> l = new LinkedList<FSTNode>();
//	 l.add(jmOldFST);
//	 FSTNode featOld = new FSTNonTerminal("Feature", "oldFeature", l);
//	
//	 JavaMethod jmNew = new JavaMethod(
//	 "public int [] method(){original();something();}");
//	
//	 FSTNode par = new FSTNonTerminal("nonterminal", "parent");
//	
//	 System.out.println(FSTGenComposer
//	 .compose(jmNew.getFST(), jmOldFST, par));

    }
}
