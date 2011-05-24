package cide.astgen.generated;

import generated.MethodDeclaration;
import generated.Parameter;
import generated.ParameterList;
import generated.SimplePrintVisitor;
import generated.Type1;
import generated.Type2;

import java.util.ArrayList;

import junit.framework.Assert;

import org.junit.Test;

import cide.gast.ASTStringNode;
import cide.gast.IToken;
import cide.gparser.Token;

public class TestGeneratedAST {

	protected MethodDeclaration generateAST() {
		Token nullToken = new Token();
		IToken nullIToken = new IToken() {
			public int getLength() {
				return 0;
			}

			public int getOffset() {
				return 0;
			}
		};

		ASTStringNode methodName = new ASTStringNode("foo", nullIToken);
		Parameter parameter1 = new Parameter(new Type2(nullToken, nullToken),
				new ASTStringNode("a", nullIToken), nullToken, nullToken);
		Parameter parameter2 = new Parameter(new Type2(nullToken, nullToken),
				new ASTStringNode("b", nullIToken), nullToken, nullToken);
		ArrayList<Parameter> parameters = new ArrayList<Parameter>();
		parameters.add(parameter1);
		parameters.add(parameter2);
		ParameterList parameterList = new ParameterList(parameters, nullToken,
				nullToken);
		return new MethodDeclaration(new Type1(nullToken, nullToken),null,
				methodName, parameterList, null, nullToken, nullToken);
	}

	@Test
	public void testASTGeneration() {
		Assert.assertNotNull(generateAST());
	}
	
	@Test
	public void testASTAccessMethods() throws SecurityException, NoSuchMethodException{
		Assert.assertNotNull(MethodDeclaration.class.getMethod("getType", null));
		Assert.assertNotNull(MethodDeclaration.class.getMethod("getParameterList", null));
	}
	
	@Test
	public void testPrinter(){
		generateAST().accept(new SimplePrintVisitor(System.out));
	}
}
