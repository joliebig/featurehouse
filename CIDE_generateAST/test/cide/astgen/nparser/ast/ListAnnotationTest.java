package cide.astgen.nparser.ast;

import junit.framework.Assert;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import cide.astgen.nparser.ast.NAbstractValue.Type;

public class ListAnnotationTest {

	private NChoice choice;
	private NNonTerminal nonTerminal;

	@Before
	public void setUp() throws Exception {
		NGrammar grammar = new NGrammar("");
		NProduction production = new NProduction(grammar, "ProductionName");
		choice = new NChoice(production);

		nonTerminal = new NNonTerminal(choice, Type.ONE, "Name");
	}

	@Test
	public void testIsListMember() {
		Assert.assertFalse(nonTerminal.isListElement());

		nonTerminal.innerPreTokens.add("&LI");
		Assert.assertTrue(nonTerminal.isListElement());
	}

	@Test
	public void testChoiceList() {
		Assert.assertFalse(choice.isList());

		NNonTerminal nt = new NNonTerminal(choice, Type.ONE, "Name2");
		nt.innerPreTokens.add("&LI");
		choice.units.add(nt);
		Assert.assertTrue(choice.isList());
	}

	@Test
	public void testGetListType() {
		Assert.assertEquals(null, choice.getListType());

		NNonTerminal nt = new NNonTerminal(choice, Type.ONE, "Name2");
		nt.innerPreTokens.add("&LI");
		choice.units.add(nt);

		Assert.assertEquals("Name2", choice.getListType());

		nt = new NNonTerminal(choice, Type.ZEROORMORE, "Name2");
		nt.innerPreTokens.add("&LI");
		choice.units.add(nt);

		Assert.assertEquals("Name2", choice.getListType());

		boolean pass = false;
		try {
			nt = new NNonTerminal(choice, Type.ONE, "Name3");
			nt.innerPreTokens.add("&LI");
			choice.units.add(nt);
			choice.getListType();
		} catch (AssertionError e) {
			pass = true;
		}
		if (!pass)
			Assert.fail();

	}

	@Test
	public void testGetListAccessMethod() {
		Assert.assertEquals(null, choice.getListAccessMethod());

		NNonTerminal nt = new NNonTerminal(choice, Type.ONE, "Name2");
		nt.innerPreTokens.add("&LI");
		choice.units.add(nt);

		Assert.assertEquals("getName2", choice.getListAccessMethod());
		nt = new NNonTerminal(choice, Type.ZEROORMORE, "Name2");
		nt.innerPreTokens.add("&LI");
		choice.units.add(nt);

		Assert.assertEquals("getName2", choice.getListAccessMethod());		
	}

}
