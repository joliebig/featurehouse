package de.ovgu.cide.fstgen;


import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;

import cide.astgen.nparser.ast.NGrammar;
import cide.astgen.nparser.parser.ParseException;

public class FSTInlineVisitorTest extends AbstractNParser{

	private FSTInlineVisitor inliner;

	@Before
	public void setUp() throws Exception {
		inliner=new FSTInlineVisitor();
	}

	@Test
	public void testInlining() throws ParseException{
		NGrammar g=parse("x:y; y:a b;");
		Assert.assertEquals(1, g.productions.get(0).getChoices().get(0).units.size());

		g=parse("@FSTNonTerminal x:y; @FSTInline y:a b;");
		Assert.assertEquals(1, g.productions.get(0).getChoices().get(0).units.size());
		g.accept(inliner);
		Assert.assertEquals(2, g.productions.get(0).getChoices().get(0).units.size());
	}
	
	@Test
	public void testRecursive() throws ParseException{
		NGrammar g = parse("@FSTNonTerminal x:y z; @FSTInline z:a b;@FSTInline a:m n;");
		Assert.assertEquals(2, g.productions.get(0).getChoices().get(0).units.size());
		g.accept(inliner);
		Assert.assertEquals(4, g.productions.get(0).getChoices().get(0).units.size());
		Assert.assertEquals("y", g.productions.get(0).getChoices().get(0).units.get(0).getName());
		Assert.assertEquals("m", g.productions.get(0).getChoices().get(0).units.get(1).getName());
		Assert.assertEquals("n", g.productions.get(0).getChoices().get(0).units.get(2).getName());
		Assert.assertEquals("b", g.productions.get(0).getChoices().get(0).units.get(3).getName());
	}
	
}
