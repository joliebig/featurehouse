package cide.astgen.nparser.visitor;

import java.util.List;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;

import cide.astgen.WhiteSpaceTest;
import cide.astgen.nparser.ast.NChoice;
import cide.astgen.nparser.ast.NGrammar;
import cide.astgen.nparser.ast.NNonTerminal;
import cide.astgen.nparser.ast.NProduction;
import cide.astgen.nparser.ast.NValue;
import cide.astgen.nparser.ast.NAbstractValue.Type;

public class ReferenceTest extends WhiteSpaceTest {

	private NValue valueNode;
	private NNonTerminal nonTerminal;
	private NGrammar grammar;
	private NProduction production1;
	private NProduction production2;
	private NChoice choice1;
	private NChoice choice2;

	@Before
	public void setUp() throws Exception {
		super.setUp();
		grammar = new NGrammar("");
		production1 = new NProduction(grammar, "Prod");
		grammar.productions.add(production1);
		choice1 = new NChoice(production1);
		production1.addChoice(choice1);
		valueNode = new NValue(choice1, Type.ONE, "IDENTIFIER");
		valueNode.innerPostTokens.add("\":\"");
		valueNode.innerPreTokens.add("@RefTarget(Grammar)");
		valueNode.innerPreTokens.add("@OtherAnnotation()");
		choice1.units.add(valueNode);
		nonTerminal = new NNonTerminal(choice1, Type.ONE, "Unit");
		nonTerminal.innerPostTokens.add("\";\"");
		// nonTerminal.innerPreTokens.add("@Reference(Grammar)");
		choice1.units.add(nonTerminal);

		production2 = new NProduction(grammar, "Unit");
		grammar.productions.add(production2);
		choice2 = new NChoice(production2);
		production2.addChoice(choice2);
		NValue valueNode2 = new NValue(choice1, Type.ONE, "IDENTIFIER");
		valueNode2.innerPreTokens.add("@Reference(Grammar)");
		choice2.units.add(valueNode2);
	}

	// @Test
	// public void referenceManagerCreation() {
	// grammar.accept(new SlimPrintVisitor(super.printStream));
	// assertWhiteSpaceEqual("Prod: <IDENTIFIER> \":\" Unit \";\";",
	// getOutput());
	// }
	@Test
	public void testCorrectSetup1() {
		production1.accept(new SlimPrintVisitor(super.printStream));
		assertWhiteSpaceEqual(
				"Prod: @RefTarget(Grammar) @OtherAnnotation() <IDENTIFIER> \":\" Unit \";\";",
				getOutput());
	}

	@Test
	public void testCorrectSetup2() {
		production2.accept(new SlimPrintVisitor(super.printStream));
		assertWhiteSpaceEqual("Unit: @Reference(Grammar) <IDENTIFIER>;",
				getOutput());
	}

	@Test
	public void testGetAnnotations() {
		List<String> annotations = valueNode.getAnnotations();
		Assert.assertEquals(2, annotations.size());
		Assert.assertEquals("@RefTarget(Grammar)", annotations.get(0));

		annotations = valueNode.getAnnotations("RefTarget");
		Assert.assertEquals(1, annotations.size());
		Assert.assertEquals("@RefTarget(Grammar)", annotations.get(0));

		annotations = valueNode.getAnnotationValues("RefTarget");
		Assert.assertEquals(1, annotations.size());
		Assert.assertEquals("Grammar", annotations.get(0));

		annotations = valueNode.getAnnotations("RefX");
		Assert.assertEquals(0, annotations.size());

		annotations = choice1.collectAnnotationValues("RefTarget");
		Assert.assertEquals(1, annotations.size());
		Assert.assertEquals("Grammar", annotations.get(0));
	}

	@Test
	public void referenceManagerCreation() {
		CreateReferenceManagerVisitor manager = new CreateReferenceManagerVisitor(
				super.printStream, "");
		grammar.accept(manager);
		Assert.assertEquals(1, manager.referenceSources.size());
		Assert.assertEquals(1, manager.referenceSources.size());
		Assert.assertTrue(manager.isConsistent());

		System.out.println(getOutput());
		assertWhiteSpaceEqual(
				"import java.util.*;"
						+ "import cide.greferences.*;"
						+ "import cide.gast.*;"
						+ "public class ReferenceManager implements IReferenceManager {"
						+ "public final static ReferenceType grammar = new ReferenceType(\"Grammar\","
						+ " new Class[] { Unit.class }, "
						+ "new Class[] { Prod.class });"
						+ "public ReferenceType[] getReferenceTypes() {"
						+ "return new ReferenceType[] { grammar };}}",
				getOutput());
	}

	@Test
	public void generateGetReferencesMethod() {
		ASTCreationVisitor v = new ASTCreationVisitor(null, "");
		String r = v.generateReferenceMethod(choice1);
		assertWhiteSpaceEqual("", r);

		r = v.generateReferenceMethod(choice2);
		assertWhiteSpaceEqual(
				"public IReferenceType[] getReferenceTypes() { "
						+ "return new IReferenceType[] { ReferenceManager.grammar }; }",
				r);
	}

	@Test
	public void testGenTypeName() {
		Assert.assertEquals("name", CreateReferenceManagerVisitor
				.genName("Name"));
	}
}
