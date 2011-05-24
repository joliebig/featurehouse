package cide.astgen.generated;

import generated.MethodDeclaration;
import generated.Name;
import generated.NameList;
import generated.Parameter;
import generated.ParameterList;
import generated.SimplePrintVisitor;
import generated.Type1;
import generated.Type2;

import java.util.ArrayList;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;

import cide.astgen.WhiteSpaceTest;
import cide.gast.ASTNode;
import cide.gast.ASTStringNode;
import cide.gast.ASTTextNode;
import cide.gast.IToken;
import cide.gparser.Token;

public class TestGeneratedPrinter extends WhiteSpaceTest {

	private ASTStringNode methodName;
	private Parameter parameter1;
	private Parameter parameter2;
	private ParameterList parameterList;
	private MethodDeclaration methodDeclaration;
	private Name throws1;
	private Name throws2;
	private Name throws3;
	private NameList nameList;
	private ASTTextNode staticToken;
	private Token nullToken;

	@Before
	public void generateAST() {
		nullToken = new Token();
		IToken nullIToken = new IToken() {
			public int getLength() {
				return 0;
			}

			public int getOffset() {
				return 0;
			}
		};

		methodName = new ASTStringNode("foo", nullIToken);
		parameter1 = new Parameter(new Type2(nullToken, nullToken),
				new ASTStringNode("a", nullIToken), nullToken, nullToken);
		parameter2 = new Parameter(new Type2(nullToken, nullToken),
				new ASTStringNode("b", nullIToken), nullToken, nullToken);
		ArrayList<Parameter> parameters = new ArrayList<Parameter>();
		parameters.add(parameter1);
		parameters.add(parameter2);
		parameterList = new ParameterList(parameters, nullToken, nullToken);

		throws1 = new Name(new ASTStringNode("X", nullIToken), nullToken,
				nullToken);
		throws2 = new Name(new ASTStringNode("Y", nullIToken), nullToken,
				nullToken);
		throws3 = new Name(new ASTStringNode("Z", nullIToken), nullToken,
				nullToken);
		ArrayList<Name> throwsList = new ArrayList<Name>();
		throwsList.add(throws1);
		throwsList.add(throws2);
		throwsList.add(throws3);
		nameList = new NameList(throwsList, nullToken, nullToken);
		staticToken = new ASTTextNode("static", nullIToken);

		methodDeclaration = new MethodDeclaration(new Type1(nullToken,
				nullToken), null, methodName, parameterList, nameList,
				nullToken, nullToken);
	}

	@Test
	public void printSimpleElements() {
		assertPrinterOutput(methodName, "foo");
		assertPrinterOutput(throws1, "X");
		assertPrinterOutput(parameter1, "int a");
		assertPrinterOutput(parameterList, "int a, int b");
		assertPrinterOutput(nameList, "X, Y, Z");
		assertPrinterOutput(methodDeclaration,
				"void foo(int a, int b) throws X, Y, Z{}");
	}

	@Test
	public void printRemovedListElements() {
		Assert.assertTrue(throws1.isOptional());
		throws1.remove();
		assertPrinterOutput(nameList, "Y, Z");
		assertPrinterOutput(methodDeclaration,
				"void foo(int a, int b) throws Y, Z{}");

		Assert.assertTrue(throws3.isOptional());
		throws3.remove();
		assertPrinterOutput(nameList, "Y");
		assertPrinterOutput(methodDeclaration,
				"void foo(int a, int b) throws Y {}");

		Assert.assertTrue(throws2.isOptional());
		throws2.remove();
		assertPrinterOutput(nameList, "");
		assertPrinterOutput(methodDeclaration, "void foo(int a, int b) {}");
	}

	@Test
	public void printStaticElements() {
		MethodDeclaration staticMethodDeclaration = new MethodDeclaration(new Type1(nullToken,
				nullToken), staticToken, methodName, parameterList, nameList,
				nullToken, nullToken);

		assertPrinterOutput(staticMethodDeclaration,
				"void static foo(int a, int b) throws X, Y, Z{}");
	}

	private void assertPrinterOutput(ASTNode node, String expectedString) {
		SimplePrintVisitor v = new SimplePrintVisitor();
		node.accept(v);
		assertWhiteSpaceEqual(expectedString, v.getResult());
	}

}
