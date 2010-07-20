package de.ovgu.cide.fstgen.ast;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Stack;

import cide.gparser.Token;

public class AbstractFSTParser {

	private static class Context {
		Context(boolean isInTerminal) {
			this.isInTerminal = isInTerminal;
		}

		List<FSTNode> children = new ArrayList<FSTNode>();
		List<Replacement> nameReplacements = new ArrayList<Replacement>();
		List<Replacement> composeReplacements = new ArrayList<Replacement>();
		boolean isInTerminal = false;
	}

	private static class Replacement {
		Replacement(String c, FSTInfo v) {
			productionName = c;
			value = v;
		}

		String productionName;
		FSTInfo value;

		public String toString() {
			return productionName + "-" + value.exportedName;
		}
	}

	static enum FSTInfoType {
		NAME, COMPOSE_OR_MERGE
	};

	protected static class FSTInfo {
		public FSTInfo(String type, String exportedName) {
			this.exportedName = exportedName;
			this.exportedComposition = "";
			this.type = type;
		}

		public FSTInfo(String type, String exportedName,
				String exportedComposition) {
			this.exportedName = exportedName;
			this.exportedComposition = exportedComposition;
			this.type = type;
		}

		final String exportedName;
		final String exportedComposition;
		final String type;

		public String toString() {
			return "[" + type + " - " + exportedName + " - "
					+ exportedComposition + "]";
		}
	}

	private HashMap<String, String> composeReplacements = new HashMap<String, String>();

	private Stack<Context> currentContext = new Stack<Context>();

	// memorize all FSTNonTerminals that are created
	public static ArrayList<FSTNode> fstnodes = new ArrayList<FSTNode>();

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

	protected FSTInfo productionEndNonTerminal(String type, String namePattern,
			String exportNamePattern) {
		Context c = currentContext.pop();

		String exportName = applyReplacements(exportNamePattern,
				c.nameReplacements, FSTInfoType.NAME);
		type = applyReplacements(type, c.nameReplacements, FSTInfoType.NAME);

		if (!cc().isInTerminal) {
			String name = namePattern.equals(exportNamePattern) ? exportName
					: applyReplacements(namePattern, c.nameReplacements,
							FSTInfoType.NAME);

			FSTNonTerminal nonTerminal = new FSTNonTerminal(type, name,
					c.children);
			fstnodes.add(nonTerminal);
			cc().children.add(nonTerminal);
		}
		return new FSTInfo(type, exportName);
	}

	private String applyReplacements(String name, List<Replacement> replList,
			FSTInfoType type) {

		for (Replacement replacement : replList) {
			String value;
			if (type == FSTInfoType.NAME)
				value = replacement.value.exportedName;
			else
				// if (type==FSTInfoType.COMPOSITION)
				value = replacement.value.exportedComposition;

			String listName = "{" + replacement.productionName + "}^";
			int listStart = name.indexOf(listName);
			// is list
			if (listStart >= 0) {
				char sepChar = name.charAt(listStart + listName.length());
				name = name.replace(listName, value
						+ (sepChar == '~' ? "" : sepChar) + listName);
			} else {
				// no list
				name = name.replace("{" + replacement.productionName + "}",
						value);
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

	/**
	 * added only for backward compatibility. do not call
	 * 
	 * @deprecated
	 */
	protected FSTInfo productionEndTerminal(String type, String namePattern,
			String exportNamePattern, String compositionMechanism, Token first,
			Token last) {
		return productionEndTerminal(type, namePattern, exportNamePattern,
				compositionMechanism, FSTTerminal.defaultMergingMechanism,
				first, last);
	}

	protected FSTInfo productionEndTerminal(String type, String namePattern,
			String exportNamePattern, String compositionMechanism,
			String mergingMechanism, Token first, Token last) {
		AbstractFSTParser.Context c = currentContext.pop();

		String prefix = getPrefix(first);
		String body = getBody(first, last);
		c.nameReplacements.add(new Replacement("TOSTRING", new FSTInfo(type,
				stripWhitespace(body))));

		String exportName = applyReplacements(exportNamePattern,
				c.nameReplacements, FSTInfoType.NAME);
		type = applyReplacements(type, c.nameReplacements, FSTInfoType.NAME);
		compositionMechanism = applyReplacements(compositionMechanism,
				c.nameReplacements, FSTInfoType.COMPOSE_OR_MERGE);
		mergingMechanism = applyReplacements(mergingMechanism,
				c.nameReplacements, FSTInfoType.COMPOSE_OR_MERGE);

		if (!c.isInTerminal) {
			String name = namePattern.equals(exportNamePattern) ? exportName
					: applyReplacements(namePattern, c.nameReplacements,
							FSTInfoType.NAME);

			cc().children.addAll(c.children);
			if (first != null)
				cc().children.add(new FSTTerminal(type, name, body, prefix,
						compositionMechanism, mergingMechanism));
		}
		return new FSTInfo(type, exportName, compositionMechanism);
	}

	private String applyComposeReplacements(String compositionMechanism) {
		if (compositionMechanism.length() > 0
				&& compositionMechanism.charAt(0) == '{') {

			for (String productionKey : composeReplacements.keySet()) {

				compositionMechanism = compositionMechanism.replace("{"
						+ productionKey + "}",
						composeReplacements.get(productionKey));

			}
		}
		return compositionMechanism;
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
			result.insert(0, t.image);
			t = t.specialToken;
		}
		return result.toString();
	}

	protected void replaceName(FSTInfo value) {
		cc().nameReplacements.add(new Replacement(value.type, value));
	}

	protected void replaceName(String name, FSTInfo value) {
		cc().nameReplacements.add(new Replacement(name, value));
	}
}
