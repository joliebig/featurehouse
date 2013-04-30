package de.ovgu.cide.fstgen.ast;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class AbstractFSTPrintVisitor extends FSTVisitor {

	private final StringBuffer buffer = new StringBuffer();
	private final PrintStream outputStream;

	private List<String> tokensInCurrentLine = new ArrayList<String>();
	private int indentInCurrentLine = 0;

	public AbstractFSTPrintVisitor(PrintStream out) {
		this.outputStream = out;
	}

	public AbstractFSTPrintVisitor() {
		this.outputStream = null;
	}

	protected void printToken(String token) {
		if (tokensInCurrentLine.size() == 0)
			indentInCurrentLine = indent;
		tokensInCurrentLine.add(token);
	}

	protected void hintIncIndent() {
		indent++;
	}

	protected void hintDecIndent() {
		indent--;
	}

	protected void hintNewLine() {
		closeLine();
	}

	protected void hintSingleSpace() {
		printToken(" ");
	}

	public boolean generateSpaces = true;

	private void closeLine() {
		for (int i = 0; i < indentInCurrentLine; i++)
			print('\t');

		String lastToken = null;
		Iterator<String> i = tokensInCurrentLine.iterator();
		while (i.hasNext()) {
			String token = i.next();
			if (generateSpaces)
				if (lastToken != null
						&& !getNoSpaceAfterToken().contains(lastToken)
						&& !getNoSpaceBeforeToken().contains(token))
					print(' ');

			print(token);
			lastToken = token;
		}

		print('\n');

		tokensInCurrentLine.clear();
		indentInCurrentLine = 0;
	}

	/**
	 * to not use if you intent to print tokens
	 * 
	 * @param s
	 */
	protected void print(String s) {
		buffer.append(s);
		if (outputStream != null)
			outputStream.print(s);
	}

	private void print(char s) {
		buffer.append(s);
		if (outputStream != null)
			outputStream.print(s);
	}

	private int indent = 0;

	public String getResult() {
		if (tokensInCurrentLine.size() > 0)
			closeLine();
		if (outputStream != null) {
			outputStream.close();
		}
		return buffer.toString();
	}

	private List<String> noSpaceAfterToken;

	protected List<String> getNoSpaceAfterToken() {
		if (noSpaceAfterToken == null) {
			noSpaceAfterToken = new ArrayList<String>();
			noSpaceAfterToken.add("{");
			noSpaceAfterToken.add("(");
			noSpaceAfterToken.add("[");
			noSpaceAfterToken.add("@");
			noSpaceAfterToken.add(".");
		}
		return noSpaceAfterToken;
	}

	private List<String> noSpaceBeforeToken;

	protected List<String> getNoSpaceBeforeToken() {
		if (noSpaceBeforeToken == null) {
			noSpaceBeforeToken = new ArrayList<String>();
			noSpaceBeforeToken.add("}");
			noSpaceBeforeToken.add(")");
			noSpaceBeforeToken.add("]");
			noSpaceBeforeToken.add(";");
			noSpaceBeforeToken.add(".");
		}
		return noSpaceBeforeToken;
	}

	protected abstract boolean isSubtype(String type, String expectedType);

	protected List<FSTNode> getChildren(FSTNonTerminal nonTerminal,
			String childType) {
		List<FSTNode> result = new ArrayList<FSTNode>();
		for (FSTNode node : nonTerminal.getChildren()) {
			if (isSubtype(node.getType(), childType))
				result.add(node);
		}
		return result;
	}

	protected FSTNode getChild(FSTNonTerminal nonTerminal, String childType) {
		List<FSTNode> result = getChildren(nonTerminal, childType);
		if (result.size() > 1) {
			throw new RuntimeException(
					"Cannot handle multple FST nodes of type " + childType
							+ " here (" + result.get(0).getName() + ":" + result.get(0).getType() + " and " + result.get(1).getName() + ":" + result.get(0).getType() + " parent: " + nonTerminal.toString() +  ")");
		}
		if (result.size() == 1)
			return result.get(0);
		return null;
	}

	@Override
	public boolean visit(FSTTerminal terminal) {
		printFeatures(terminal, true);
	//if(CommandLineParameterHelper.isJML())
		handleJMLReplacements(terminal);
		printToken(terminal.getSpecialTokenPrefix()+terminal.getBody());
		printFeatures(terminal, false);
		// hintNewLine();
		return false;
	}

	private void handleJMLReplacements(FSTTerminal terminal) {
		if (terminal.getType().equals("MethodDecl")) {

			terminal.setBody(handleJMLModifierMethod(terminal.getBody()));
		}
		if (terminal.getType().equals("ConstructorDecl")) {

			terminal.setBody(handleJMLModifierMethod(terminal.getBody()));
		}
		if (terminal.getType().equals("ModFieldDeclaration")) {

			terminal.setBody(handleJMLModifierField(terminal.getBody()));
		}
		if (terminal.getType().equals("Modifiers2")) {

			terminal.setBody(handleJMLModifierClass(terminal.getBody()));
		}
		if (terminal.getType().equals("Modifiers")) {

			terminal.setBody(handleJMLModifierClass(terminal.getBody()));
		}
		if (terminal.getType().equals("ParamModifier")) {
			terminal.setBody(handleJMLModifierClass(terminal.getBody()));
		}
		if( terminal.getType().equals("ImportDeclaration1")){
			terminal.setBody("/*@" + terminal.getBody() + "@*/");
		}
		if (terminal.getType().equals("AssertStatement")
				|| terminal.getType().equals("JMLAnnotationStatement ")
				|| terminal.getType().equals("ModelProgStatement ")) {
			
			terminal.setBody("/*@" + terminal.getBody() + "@*/");
		}
	}


	protected void printFeatures(FSTNode node, boolean b) {
		// used only in subclasses		
	}

	private String handleJMLModifierMethod(String in) {
		System.out.println("methodin "+in);
		List<String> JMLModifiers = new ArrayList<String>();
		JMLModifiers.add("SPEC_PUBLIC".toLowerCase());
		JMLModifiers.add("SPEC_PROTECTED".toLowerCase());
		JMLModifiers.add("MODEL".toLowerCase());
		JMLModifiers.add("GHOST".toLowerCase());
		JMLModifiers.add("PURE".toLowerCase());
		JMLModifiers.add("HELPER".toLowerCase());
		JMLModifiers.add("INSTANCE".toLowerCase());
		JMLModifiers.add("UNINITIALIZED".toLowerCase());
		JMLModifiers.add("SPEC_JAVA_MATH".toLowerCase());
		JMLModifiers.add("SPEC_SAFE_MATH".toLowerCase());
		JMLModifiers.add("SPEC_BIGINT_MATH".toLowerCase());
		JMLModifiers.add("CODE_JAVA_MATH".toLowerCase());
		JMLModifiers.add("CODE_SAFE_MATH".toLowerCase());
		JMLModifiers.add("CODE_BIGINT_MATH".toLowerCase());
		JMLModifiers.add("NON_NULL".toLowerCase());
		JMLModifiers.add("NULLABLE".toLowerCase());
		JMLModifiers.add("NULLABLE_BY_DEFAULT".toLowerCase());
		JMLModifiers.add("EXTRACT".toLowerCase());

		String out = in.substring(0, in.indexOf(")"));
		   Pattern p = Pattern.compile("\\smodel\\s"); 
		    Matcher m = p.matcher(in); 
		if (m.find())
			return out = "/*@" + in + "@*/";
		for (String mod : JMLModifiers) {
			out = out.replaceAll("(^|\\W)"+mod+"\\s", " /*@" + mod + "@*/ ");
		}
		
		out = out + in.substring(in.indexOf(")"));
		System.out.println("method "+out);
		return out;
	}

	private String handleJMLModifierField(String in) {

		List<String> JMLModifiers = new ArrayList<String>();
		JMLModifiers.add("SPEC_PUBLIC".toLowerCase());
		JMLModifiers.add("SPEC_PROTECTED".toLowerCase());
		JMLModifiers.add("MODEL".toLowerCase());
		JMLModifiers.add("GHOST".toLowerCase());
		JMLModifiers.add("PURE".toLowerCase());
		JMLModifiers.add("HELPER".toLowerCase());
		JMLModifiers.add("INSTANCE".toLowerCase());
		JMLModifiers.add("UNINITIALIZED".toLowerCase());
		JMLModifiers.add("SPEC_JAVA_MATH".toLowerCase());
		JMLModifiers.add("SPEC_SAFE_MATH".toLowerCase());
		JMLModifiers.add("SPEC_BIGINT_MATH".toLowerCase());
		JMLModifiers.add("CODE_JAVA_MATH".toLowerCase());
		JMLModifiers.add("CODE_SAFE_MATH".toLowerCase());
		JMLModifiers.add("CODE_BIGINT_MATH".toLowerCase());
		JMLModifiers.add("NON_NULL".toLowerCase());
		JMLModifiers.add("NULLABLE".toLowerCase());
		JMLModifiers.add("NULLABLE_BY_DEFAULT".toLowerCase());
		JMLModifiers.add("EXTRACT".toLowerCase());

		String out = null;
		   Pattern p = Pattern.compile("\\smodel\\s"); 
		    Matcher m = p.matcher(in); 
		if (m.find())
			out = "/*@" + in + "@*/";
		else {
			out = in;
			for (String mod : JMLModifiers) {
				out = out.replaceAll("(^|\\W)"+mod+"\\s", " /*@ " + mod + " @*/ ");
			}
		}
		System.out.println("field "+out);
		return out;
	}

	private String handleJMLModifierClass(String in) {
		List<String> JMLModifiers = new ArrayList<String>();
		JMLModifiers.add("SPEC_PUBLIC".toLowerCase());
		JMLModifiers.add("SPEC_PROTECTED".toLowerCase());
	//	JMLModifiers.add("MODEL".toLowerCase());
		JMLModifiers.add("GHOST".toLowerCase());
		JMLModifiers.add("PURE".toLowerCase());
		JMLModifiers.add("HELPER".toLowerCase());
		JMLModifiers.add("INSTANCE".toLowerCase());
		JMLModifiers.add("UNINITIALIZED".toLowerCase());
		JMLModifiers.add("SPEC_JAVA_MATH".toLowerCase());
		JMLModifiers.add("SPEC_SAFE_MATH".toLowerCase());
		JMLModifiers.add("SPEC_BIGINT_MATH".toLowerCase());
		JMLModifiers.add("CODE_JAVA_MATH".toLowerCase());
		JMLModifiers.add("CODE_SAFE_MATH".toLowerCase());
		JMLModifiers.add("CODE_BIGINT_MATH".toLowerCase());
		JMLModifiers.add("NON_NULL".toLowerCase());
		JMLModifiers.add("NULLABLE".toLowerCase());
		JMLModifiers.add("NULLABLE_BY_DEFAULT".toLowerCase());
		JMLModifiers.add("EXTRACT".toLowerCase());

		String out = in;
		for (String mod : JMLModifiers) {
			out = out.replaceAll("(^|\\W)"+mod+"(\\s|\\z)", " /*@" + mod + "@*/ ");
		}
		System.out.println("class "+out);
		return out;
	}

}
