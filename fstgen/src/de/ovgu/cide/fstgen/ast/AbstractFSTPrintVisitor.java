package de.ovgu.cide.fstgen.ast;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

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
		printToken(terminal.getSpecialTokenPrefix()+terminal.getBody());
		printFeatures(terminal, false);
		//		hintNewLine();
		return false;
	}

	protected void printFeatures(FSTNode node, boolean b) {
		// used only in subclasses		
	}
}
