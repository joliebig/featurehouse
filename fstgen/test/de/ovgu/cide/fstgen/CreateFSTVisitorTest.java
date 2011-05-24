package de.ovgu.cide.fstgen;

import java.util.Set;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import cide.astgen.nparser.ast.NGrammar;
import cide.astgen.nparser.parser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNodeType;

public class CreateFSTVisitorTest extends AbstractNParser{

	@Before
	public void setUp() throws Exception {

	}

	// @Test
	// public void testCheckAnnotations() throws Exception {
	// NGrammar grammar = parseFile("test/fj.gcide");
	// grammar.accept(v);
	// Assert.assertFalse(v.hasWellformedFSTAnnotations());
	// }
	//
	@Test
	public void testCheckWellformedAnnotationsFJ() throws Exception {
		NGrammar grammar = parseFile("test/fj_fst.gcide");
		CreateFSTVisitor v = new CreateFSTVisitor();
		grammar.accept(v);
		Assert.assertTrue(v.getWellformedErrorMsg(), v
				.hasWellformedFSTAnnotations());
	}

	@Test @Ignore
	public void testCheckWellformedAnnotationsJava() throws Exception {
		NGrammar grammar = parseFile("test/java15_fst.gcide");
		CreateFSTVisitor v = new CreateFSTVisitor();
		grammar.accept(v);
		Assert.assertTrue(v.getWellformedErrorMsg(), v
				.hasWellformedFSTAnnotations());
	}

	@Test
	public void checkUnannotatedGrammar() throws ParseException {
		checkWellformed("m:x;x:<i>;");
	}

	@Test
	public void checkTerminalAndNonTerminal() throws ParseException {
		checkIllformed("@FSTNonTerminal(name=\"abc\") @FSTTerminal m:x;x:<i>;");
	}

	@Test
	public void checkNonTerminalFromTerminal() throws ParseException {
		checkIllformed("@FSTTerminal m:x; @FSTNonTerminal(name=\"abc\") x:<i>;");
	}

	@Test
	public void checkTerminalFromNonTerminal() throws ParseException {
		checkWellformed("@FSTNonTerminal(name=\"abc\") m:x; @FSTTerminal x:<i>;");
	}

	@Test
	public void checkNonTerminalName() throws ParseException {
		checkWellformed("@FSTNonTerminal(name=\"abc\") m:x; @FSTTerminal x:<i>;");
		checkIllformed("@FSTNonTerminal m:x; @FSTTerminal x:<i>;");
	}

	@Test
	public void testTypeCollector() throws ParseException {
		Set<FSTNodeType> types;

		types = getNodeTypes("m:x; x:<i>;");
		Assert.assertEquals(1, types.size());

		types = getNodeTypes("@FSTNonTerminal(name=\"abc\") m:x; @FSTTerminal x:<i>;");
		Assert.assertEquals(2, types.size());
		int found = 0;
		for (FSTNodeType t : types) {
			if ("m".equals(t.getName())) {
				Assert.assertFalse("m is not terminal", t.isTerminal());
				found++;
			}
			if ("x".equals(t.getName())) {
				Assert.assertTrue("x is terminal", t.isTerminal());
				found++;
			}
		}
		Assert.assertTrue(found == 2);

		printTypes(types);
	}

	@Test
	public void testTypeCollectorFJ() throws Exception {
		NGrammar grammar = parseFile("test/fj_fst.gcide");
		CreateFSTVisitor v = new CreateFSTVisitor();
		grammar.accept(v);
		Set<FSTNodeType> types = v.getFSTNodeTypes();
		printTypes(types);
		int found = 0;
		for (FSTNodeType t : types) {
			if ("TypeDeclaration".equals(t.getName())) {
				Assert.assertFalse("m is not terminal", t.isTerminal());
				found++;
			}
			if ("ClassConstructor".equals(t.getName())
					|| "MethodDeclaration".equals(t.getName())
					|| "VarDeclaration".equals(t.getName())
					|| "ExtendedType1".equals(t.getName())
					|| "ExtendedType2".equals(t.getName())) {
				Assert.assertTrue("x is terminal", t.isTerminal());
				found++;
			}
		}
		Assert.assertEquals(6, found);
	}

	@Test @Ignore
	public void testTypeCollectorJava() throws Exception {
		NGrammar grammar = parseFile("test/java15_fst.gcide");
		CreateFSTVisitor v = new CreateFSTVisitor();
		grammar.accept(v);
		Set<FSTNodeType> types = v.getFSTNodeTypes();
		printTypes(types);
		// int found = 0;
		// for (FSTNodeType t : types) {
		// if ("TypeDeclarator".equals(t.getName())) {
		// Assert.assertFalse("m is not terminal", t.isTerminal());
		// found++;
		// }
		// if ("ClassConstructor".equals(t.getName())
		// || "MethodDeclaration".equals(t.getName())
		// || "VarDeclaration".equals(t.getName())
		// || "ExtendedType".equals(t.getName())) {
		// Assert.assertTrue("x is terminal", t.isTerminal());
		// found++;
		// }
		// }
		Assert.assertEquals(5, types.size());
	}

	private void printTypes(Set<FSTNodeType> types) {
//		System.out.println("-----");
//		System.out.println("Non-Terminal types:");
//		for (FSTNodeType type : types)
//			if (!type.isTerminal()) {
//				System.out.println("\t" + type.getName());
//			}
//		System.out.println("Terminal types:");
//		for (FSTNodeType type : types)
//			if (type.isTerminal()) {
//				System.out.println("\t" + type.getName());
//			}

	}

	private Set<FSTNodeType> getNodeTypes(String grammarStr)
			throws ParseException {
		NGrammar grammar = parse(grammarStr);
		CreateFSTVisitor v = new CreateFSTVisitor();
		grammar.accept(v);
		Assert.assertTrue("Not considered wellformed: " + grammarStr
				+ " Reason: " + v.getWellformedErrorMsg(), v
				.hasWellformedFSTAnnotations());
		return v.getFSTNodeTypes();
	}

	private void checkWellformed(String grammarStr) throws ParseException {
		getNodeTypes(grammarStr);
	}

	private void checkIllformed(String grammarStr) throws ParseException {
		NGrammar grammar = parse(grammarStr);
		CreateFSTVisitor v = new CreateFSTVisitor();
		grammar.accept(v);
		Assert.assertTrue("Considered wellformed: " + grammarStr, !v
				.hasWellformedFSTAnnotations());
	}

}
