package de.ovgu.cide.fstgen.ast;

import cide.gparser.Token;

import java.util.*;

public class AbstractFSTParser {

	private static class Context {
		Context(boolean isInTerminal) {
			this.isInTerminal = isInTerminal;
		}

		List<FSTNode> children = new ArrayList<FSTNode>();
		List<NameReplacement> replacements = new ArrayList<NameReplacement>();
		boolean isInTerminal = false;
	}

	private static class NameReplacement {
		NameReplacement(String c, String v) {
			choiceName = c;
			value = v;
		}

		String choiceName;
		String value;
	}

	private Stack<Context> currentContext = new Stack<Context>();

	protected AbstractFSTParser() {
		// list that finally contains the root element
		currentContext.push(new Context(false));
	}

	/**
	 * current context
	 * 
	 * @return
	 */
	private Context cc() {
		return currentContext.peek();
	}

	public FSTNode getRoot() {
		return cc().children.get(0);
	}

	/**
	 * called at the beginning of a production. at the end, either
	 * productionEndTerminal or productionEndNonTerminal must be called
	 */
	protected void productionStart(boolean isInTerminal) {
		currentContext.push(new Context(isInTerminal));
	}

	protected String productionEndNonTerminal(String type, String namePattern,
			String exportNamePattern) {
		Context c = currentContext.pop();

		String exportName = applyReplacements(exportNamePattern, c.replacements);

		if (!cc().isInTerminal) {
			String name = namePattern.equals(exportNamePattern) ? exportName
					: applyReplacements(namePattern, c.replacements);

			FSTNonTerminal nonTerminal = new FSTNonTerminal(type, name,
					c.children);
			cc().children.add(nonTerminal);
		}
		return exportName;
	}

	private String applyReplacements(String name, List<NameReplacement> replList) {

		for (NameReplacement replacement : replList) {

			String listName = "{" + replacement.choiceName + "}^";
			int listStart = name.indexOf(listName);
			// is list
			if (listStart >= 0) {
				char sepChar = name.charAt(listStart + listName.length());
				name = name.replace(listName, replacement.value
						+ (sepChar == '~' ? "" : sepChar) + listName);
			} else {
				// no list
				name = name.replace("{" + replacement.choiceName + "}",
						replacement.value);
			}
		}

		while (name.contains("{AUTO}")) {
			name = name.replace("{AUTO}", generateName());
		}

		name = name.replaceAll("(.)\\{[^\\{\\}]*?\\}\\^\\1", "");
		name = name.replaceAll("\\{[^\\{\\}]*?\\}\\^.", "");

		return name;

	}

	private static int uniqueId = 0;

	private CharSequence generateName() {
		return "auto" + (++uniqueId);
	}

	protected String productionEndTerminal(String type, String namePattern,
			String exportNamePattern, String compositionMechanism, Token first,
			Token last) {
		AbstractFSTParser.Context c = currentContext.pop();

		String prefix = getPrefix(first);
		String body = getBody(first, last);
		c.replacements.add(new NameReplacement("TOSTRING",
				stripWhitespace(body)));

		String exportName = (applyReplacements(exportNamePattern,
				c.replacements));

		if (!c.isInTerminal) {
			String name = namePattern.equals(exportNamePattern) ? exportName
					: applyReplacements(namePattern, c.replacements);

			cc().children.addAll(c.children);
			if (first != null)
				cc().children.add(new FSTTerminal(type, name, body, prefix,
						compositionMechanism));
		}
		return exportName;
	}

	private String stripWhitespace(String body) {
		return body.replaceAll("\\s", "");
	}

	/**
	 * returns the text between first and last token, but NOT the special tokens
	 * of first
	 * 
	 * @param token
	 * @param last
	 * @return
	 */
	private String getBody(Token first, Token last) {
		StringBuffer body = new StringBuffer();
		Token token = first;
		if (token != null) {
			while (token != null && token != last.next) {
				if (token.specialToken != null && token != first) {
					body.append(getPrefix(token));
				}
				body.append(token.image);

				token = token.next;
			}
		}
		return body.toString();
	}

	private String getPrefix(Token token) {
		StringBuffer result = new StringBuffer();
		Token t = token.specialToken;
		while (t != null) {
			result.append(t.image);
			t = t.next;
		}
		return result.toString();
	}

	protected void replaceName(String choiceName, String value) {
		cc().replacements.add(new NameReplacement(choiceName, value));
	}
}
