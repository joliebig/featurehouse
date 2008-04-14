package de.ovgu.cide.fstgen;

import java.io.FileInputStream;
import java.io.FileNotFoundException;

import org.junit.Before;
import org.junit.Test;

import tmp.generated_java15.Java15Parser;
import tmp.generated_javacc.JavaCCParser;
import tmp.generated_xml.XMLParser;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.AbstractFSTPrintVisitor;
import de.ovgu.cide.fstgen.ast.FSTNode;

public class PrettyPrinterTests {

	@Before
	public void setUp() throws Exception {
	}

	/**
	 * @param args
	 * @throws FileNotFoundException
	 * @throws ParseException
	 */
	@Test
	public void testJavaCC() throws FileNotFoundException, ParseException {
		JavaCCParser parser = new JavaCCParser(new OffsetCharStream(
				new FileInputStream("test/C.jj")));
		parser.javacc_input(false);
		FSTNode fst = parser.getRoot();
		AbstractFSTPrintVisitor p = new tmp.generated_javacc.SimplePrintVisitor();
		fst.accept(p);
		System.out.println(p.getResult());
	}

	@Test
	public void testXML() throws FileNotFoundException, ParseException {
		XMLParser parser = new XMLParser(new OffsetCharStream(
				new FileInputStream("test/test.xml")));
		parser.Document(false);
		FSTNode fst = parser.getRoot();
		AbstractFSTPrintVisitor p = new tmp.generated_xml.SimplePrintVisitor();
		fst.accept(p);
		System.out.println(fst.printFST(0));
		System.out.println(p.getResult());
	}
	
	@Test
	public void testJava() throws FileNotFoundException, ParseException {
		Java15Parser parser = new Java15Parser(new OffsetCharStream(
				new FileInputStream("test/Test.java")));
		parser.CompilationUnit(false);
		FSTNode fst = parser.getRoot();
		AbstractFSTPrintVisitor p = new tmp.generated_java15.SimplePrintVisitor();
		fst.accept(p);
		System.out.println(p.getResult());
	}

}
