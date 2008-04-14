package cide.astgen.nparser.ast;

import static org.junit.Assert.fail;

import java.util.ArrayList;

import junit.framework.Assert;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import cide.astgen.nparser.ast.NAbstractValue.Type;
import cide.gast.ASTNode;
import cide.gast.ASTStringNode;
import cide.gast.IToken;
import cide.gast.PropertyOne;
import cide.gast.PropertyZeroOrMore;

public class NAbstractValueTest {

	private NNonTerminal nonTerminal;
	private NNonTerminal nonTerminalLower;
	private NNonTerminal nonTerminalZeroOrMore;
	private NNonTerminal nonTerminalWrapper;

	@Before
	public void setUp() throws Exception {
		NGrammar grammar = new NGrammar("");
		NProduction production = new NProduction(grammar, "ProductionName");
		NChoice choice1 = new NChoice(production);
		NChoice choice2 = new NChoice(production);

		nonTerminal = new NNonTerminal(choice1, Type.ONE, "Name");
		nonTerminalLower = new NNonTerminal(choice1, Type.ONE, "name");
		nonTerminalZeroOrMore = new NNonTerminal(choice1, Type.ZEROORMORE, "Name");
		nonTerminalWrapper = new NNonTerminal(choice1, Type.ONE, "Name");
		nonTerminalWrapper.setWrapsAroundType("Wrappee");
	}

	@After
	public void tearDown() throws Exception {
	}
	
	@Test
	public void testMultiTypes() {
		Assert.assertEquals(false, NAbstractValue.isMultiType(Type.ONE));
		Assert.assertEquals(false, NAbstractValue.isMultiType(Type.ZEROORONE));
		Assert.assertEquals(true, NAbstractValue.isMultiType(Type.ZEROORMORE));
		Assert.assertEquals(true, NAbstractValue.isMultiType(Type.ONEORMORE));
		Assert.assertEquals(false, NAbstractValue
				.isMultiType(Type.OPTIONALWITHDEFAULT));
		Assert.assertEquals(true, NAbstractValue.isMultiType(Type.LIST));
	}

	@Test
	public void testGetName() {
		Assert.assertEquals("Name", nonTerminal.getName());
		Assert.assertEquals("name", nonTerminalLower.getName());
		Assert.assertEquals("Name", nonTerminalZeroOrMore.getName());
		Assert.assertEquals("Name", nonTerminalWrapper.getName());
	}

	@Test
	public void testGenVariableType() {
		Assert.assertEquals("Name", nonTerminal.genVariableType());
		Assert.assertEquals("name", nonTerminalLower.genVariableType());
		Assert.assertEquals("ArrayList<Name>", nonTerminalZeroOrMore.genVariableType());
		Assert.assertEquals("ASTNode", nonTerminalWrapper.genVariableType());
	}

	@Test
	public void testGenVariablePlainType() {
		Assert.assertEquals("Name", nonTerminal.genVariablePlainType());
		Assert.assertEquals("name", nonTerminalLower.genVariablePlainType());
		Assert.assertEquals("Name", nonTerminalZeroOrMore.genVariablePlainType());
		Assert.assertEquals("ASTNode", nonTerminalWrapper.genVariablePlainType());
	}

	@Test
	public void testGenVariableName() {
		Assert.assertEquals("name", nonTerminal.genVariableName());
		Assert.assertEquals("name", nonTerminalLower.genVariableName());
		Assert.assertEquals("nameList", nonTerminalZeroOrMore.genVariableName());
		Assert.assertEquals("name", nonTerminalWrapper.genVariableName());
	}

	@Test
	public void testGenVariablePlainName() {
		Assert.assertEquals("name", nonTerminal.genVariablePlainName());
		Assert.assertEquals("name", nonTerminalLower.genVariablePlainName());
		Assert.assertEquals("name", nonTerminalZeroOrMore.genVariablePlainName());
		Assert.assertEquals("name", nonTerminalWrapper.genVariablePlainName());
	}

	@Test
	public void testGenPropertyName() {
		Assert.assertEquals("name", nonTerminal.genPropertyName());
		Assert.assertEquals("name", nonTerminalLower.genPropertyName());
		Assert.assertEquals("name", nonTerminalZeroOrMore.genPropertyName());
		Assert.assertEquals("name", nonTerminalWrapper.genPropertyName());
	}

	@Test
	public void testGenAccessMethod() {
		Assert.assertEquals("getName", nonTerminal.genAccessMethod());
		Assert.assertEquals("getName", nonTerminalLower.genAccessMethod());
		Assert.assertEquals("getName", nonTerminalZeroOrMore.genAccessMethod());
		Assert.assertEquals("getName", nonTerminalWrapper.genAccessMethod());
	}
	

}
