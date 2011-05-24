package cide.astgen.nparser.visitor;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import junit.framework.Assert;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import sun.reflect.ReflectionFactory.GetReflectionFactoryAction;

import cide.astgen.nparser.ast.NAbstractValue;
import cide.astgen.nparser.ast.NChoice;
import cide.astgen.nparser.ast.NGrammar;
import cide.astgen.nparser.ast.NNonTerminal;
import cide.astgen.nparser.ast.NProduction;
import cide.astgen.nparser.ast.NTextOnly;
import cide.astgen.nparser.ast.NAbstractValue.Type;

public class CreateSimplePrintVisitorVisitorTest {

	private NChoice choice;
	private ByteArrayOutputStream byteStream;
	private CreateSimplePrintVisitorVisitor visitor;

	@Before
	public void setUp() throws Exception {
		NGrammar grammar = new NGrammar("");
		NProduction production = new NProduction(grammar, "Prod");
		this.choice = new NChoice(production);
		this.byteStream = new ByteArrayOutputStream();
		this.visitor = new CreateSimplePrintVisitorVisitor(new PrintStream(
				byteStream), "");
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testSingleSimplePrint() {
		NNonTerminal value = genNonTerminal();
		visitor.visitUnit(value);
		assertWhiteSpaceEqual(NONTERMINALOUTPUT, getOutput());
	}

	private static final String NONTERMINALOUTPUT = "{Name v=n.getName();if (v!=null) {v.accept(this);}}";

	private NNonTerminal genNonTerminal() {
		return new NNonTerminal(choice, Type.ONE, "Name");
	}

	@Test
	public void testMultiSimplePrint() {
		NNonTerminal value = new NNonTerminal(choice, Type.ZEROORMORE, "Name");
		visitor.visitUnit(value);
		assertWhiteSpaceEqual("for (Name v:n.getName()){v.accept(this);}",
				getOutput());
	}

	@Test
	public void testTextAndSinglePrint() {
		NNonTerminal value = genNonTerminal();
		value.innerPreTokens.add("\"(\"");
		value.innerPostTokens.add("\")\"");
		visitor.visitUnit(value);
		assertWhiteSpaceEqual(
				"{Name v=n.getName();if (v!=null) {printToken(\"(\");v.accept(this);printToken(\")\");}}",
				getOutput());
	}

	@Test
	public void testTextAndMultiPrint() {
		NNonTerminal value = new NNonTerminal(choice, Type.ZEROORMORE, "Name");
		value.innerPreTokens.add("\"(\"");
		value.innerPostTokens.add("\")\"");
		visitor.visitUnit(value);
		assertWhiteSpaceEqual(
				"for (Name v:n.getName()){printToken(\"(\");v.accept(this);printToken(\")\");}",
				getOutput());
	}

	@Test
	public void testTextOnly() {
		NTextOnly value = new NTextOnly(choice, Type.ZEROORONE);
		value.innerPreTokens.add("\"(\"");
		value.innerPostTokens.add("\")\"");
		visitor.visitUnit(value);
		assertWhiteSpaceEqual(
				"{ASTTextNode v=n.getText1();if (v!=null) {printToken(\"(\");v.accept(this);printToken(\")\");}}",
				getOutput());
	}

	@Test
	public void testTextOnlyWithoutInnerToken() {
		NTextOnly value = new NTextOnly(choice, Type.ONE);
		value.outerPreTokens.add("\"(\"");
		value.outerPostTokens.add("\")\"");
		visitor.visitUnit(value);
		assertWhiteSpaceEqual("", getOutput());

		value = new NTextOnly(choice, Type.ZEROORMORE);
		value.outerPreTokens.add("\"(\"");
		value.outerPostTokens.add("\")\"");
		visitor.visitUnit(value);
		Assert.assertFalse("".equals(getOutput()));

		try {
			value = new NTextOnly(choice, Type.ONE);
			value.innerPreTokens.add("\"(\"");
			value.innerPostTokens.add("\")\"");
			visitor.visitUnit(value);
			Assert.fail();
		} catch (AssertionError e) {
		}
	}

	private String getOutput() {
		return byteStream.toString().trim();
	}

	private void assertWhiteSpaceEqual(String a, String b) {
		String ashort = removeWhiteSpace(a);
		String bshort = removeWhiteSpace(b);
		Assert.assertEquals(b + "\n\nexpected:\n" + a, ashort, bshort);
	}

	private String removeWhiteSpace(String a) {
		return a.replaceAll("[\\ ,\\t,\\n,\\r]", "");
	}

	@Test
	public void testEmptyChoice() {
		choice.accept(visitor);
		Assert.assertEquals("", getOutput());
	}

	@Test
	public void testSimpleChoice() {
		choice.units.add(genNonTerminal());
		choice.accept(visitor);
		assertWhiteSpaceEqual("if (node instanceof Prod){Prod n=(Prod)node;"
				+ NONTERMINALOUTPUT + "return false;}", getOutput());
	}

	private NNonTerminal addListElement(NChoice choice, Type type) {
		NNonTerminal nonTerminal = new NNonTerminal(choice, type, "LE");
		nonTerminal.innerPreTokens.add("&LI");
		Assert.assertTrue(nonTerminal.isListElement());
		choice.units.add(nonTerminal);
		return nonTerminal;
	}

	@Test
	public void testPrintListElement() {
		choice.units.add(genNonTerminal());

		visitor.visitUnit(addListElement(choice, Type.ONE));
		assertWhiteSpaceEqual(SINGLELISTELEMENTOUTPUT, getOutput());
	}

	@Test
	public void testPrintListElementMulti() {
		choice.units.add(genNonTerminal());
		visitor.visitUnit(addListElement(choice, Type.ZEROORMORE));
		assertWhiteSpaceEqual(MULTILISTELEMENTOUTPUT, getOutput());
	}

	private final static String SINGLELISTELEMENTOUTPUT = " if(listElements.hasNext()){listElements.next().accept(this);} ";
	private final static String MULTILISTELEMENTOUTPUT = " while(listElements.hasNext()){listElements.next().accept(this);} ";

	@Test
	public void testChoiceWithList() {
		choice.units.add(genNonTerminal());
		addListElement(choice, Type.ONE);
		addListElement(choice, Type.ZEROORMORE);

		choice.accept(visitor);
		// System.out.println(getOutput());
		assertWhiteSpaceEqual("if (node instanceof Prod){Prod n=(Prod)node;"
				+ "Iterator<LE> listElements = n.getLE().iterator();"
				+ NONTERMINALOUTPUT
				+ SINGLELISTELEMENTOUTPUT+
				MULTILISTELEMENTOUTPUT
				+ "return false;}", getOutput());
	}

	// @Test
	// public void testSimpleChoice() {
	// choice.units.add(genNonTerminal());
	// choice.accept(visitor);
	// System.out.println(getOutput());
	// assertWhiteSpaceEqual(
	// "if (node instanceof Prod){Prod n=(Prod)node;"+NONTERMINALOUTPUT+"return
	// false;}",
	// getOutput());
	// }

}
