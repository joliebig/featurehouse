package composer.rules.meta;



import java.util.regex.Matcher;

import composer.FSTGenComposerExtension;
import composer.rules.ContractComposition;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

/**
 * TODO description
 * @author Jens Meinicke
 *
 */
public class ContractCompositionMeta extends ContractComposition {

	public ContractCompositionMeta(String contract_style) {
		super(contract_style);
	}

	@Override
	protected String getReplacementString(FSTTerminal terminalA,
			FSTTerminal terminalB) {
		String res = super.getReplacementString(terminalA, terminalB);
		String[] baseCases = terminalB.getBody().trim().split("also");
		String orOriginal = "";
		for (String caseB : baseCases) {
			String[] clasesB = caseB.trim().split(";");
			for (String clauseB : clasesB) {
				if (!clauseB.trim().isEmpty()) {
					if (clauseB.contains(ORIGINAL_OR)) {
						orOriginal = clauseB.substring(clauseB.indexOf("\\req "));
						orOriginal = orOriginal.replaceAll("\\\\req ", "");
						clauseB = clauseB.substring(0, clauseB.indexOf("\\req "));
						res += clauseB;
					} else {
						clauseB = clauseB.replaceAll("requires ", "requires !FeatureModel." + getFeatureName(terminalA) + " ==> ");
						clauseB = clauseB.replaceAll("ensures ", "ensures !FeatureModel." + getFeatureName(terminalA) + " ==> ");
						if (clauseB.contains("requires ")) {
							res = res.replaceFirst("requires ", Matcher.quoteReplacement(clauseB) + ";\nrequires ");
						} else if (clauseB.contains("ensures ")) {
							res = res.replaceFirst("ensures ", Matcher.quoteReplacement(clauseB) + ";\nensures ");
						} else {
							res += clauseB + ";";
						}
					}
				}
			}
		}
		res = res.toString().replaceAll("\r", "");
		res = res.replaceAll("@", "");
		res = res.replaceAll("\\n", "");
		res = res.replaceAll("\\t", "");
		while (res.contains("  ")) {
			res = res.replaceAll("  ", " ");
		}
		String[] allClauses = res.split(";");
		StringBuilder result = new StringBuilder();
		for (int i  = 0; i < allClauses.length;i++) {
			result.append(allClauses[i].replaceAll("\\t", " ").replaceAll("  ", " ").trim());
			result.append(";\r\n\t");
			if (i < allClauses.length - 1) {
				result.append(" @ ");
			}
		}
		return result.toString().replace(ORIGINAL_OR, " || " + orOriginal.replaceAll("@", ""));
	}
	
	private static String getFeatureName(FSTNode node) {
		if ("Feature".equals(node.getType()))
			return node.getName().toLowerCase() + (FSTGenComposerExtension.key ? "" : "()");
		else
			return getFeatureName(node.getParent());
	}

	/**
	 * TODO description
	 */
	@Override
	public void preCompose(FSTTerminal terminal) {
		String body = terminal.getBody();
		body = body.replaceAll("\t", "");
		body = body.replaceAll("\n", "\n\t @ ");
		while (body.contains("  ")) {
			body = body.replaceAll("  ", " ");
		}

		String result = "";
		boolean added = false;
		boolean open = false;
		if (body.contains(";")) {
			for (String s : body.split(";")) {
				if (s.contains("ensures ")) {
					if (open) {
						result += ")";
						open = false;
					}
					if (added) {
						result += ";";
					}
					result = result + s.replace("ensures ", "ensures FeatureModel." + getFeatureName(terminal) + " ==> (");
					open = true;
				} else if (s.contains("requires ")) {
					if (open) {
						result += ")";
						open = false;
					}
					if (added) {
						result += ";";
					}
					result = result + s.replace("requires ", "requires FeatureModel." + getFeatureName(terminal) + " ==> (");
					open = true;
				} else if (s.contains("assignable ")) {
					if (open) {
						result += ")";
						open = false;
					}
					if (added) {
						result += ";";
					}
					result = result + s;
				} else {
					if (added) {
						result += ";";
					}
					result = result + s;
				}
				added = true;
			}
			if (open) {
				result += ")";
			}
			if (added) {
				result += ";";
			}
			body = result;
		}
	
		int ensuresIndex = body.indexOf("ensures ");
		int requiresIndex = body.indexOf("requires ");
		int index = ensuresIndex < requiresIndex ? ensuresIndex : requiresIndex;
		if (ensuresIndex == -1) {
			if (requiresIndex == -1) {
				index = 0;
			} else {
				index = requiresIndex;
			}
		} else if (requiresIndex == -1) {
			index = ensuresIndex;
		}
		
		String start = body.substring(0, index);
		String end = body.substring(index);
		terminal.setBody(start + "\\req FeatureModel." + getFeatureName(terminal) + ORIGINAL_OR + ";\r\n\t @ " + end);
	}
	 
	/**
	 * TODO description
	 */
	@Override
	public void postCompose(FSTTerminal terminal) {
		String body = terminal.getBody();
		if (FSTGenComposerExtension.key && body.replaceAll("@", "").replaceAll("\\\\[req,nreq][^;]*;", "").trim().isEmpty()) {
			terminal.setBody("");
			return;
		}
		
		body = body.replaceAll("\\\\req", "requires");
		body = body.replaceAll("\\\\nreq", "requires");
		body = body.replaceAll("\\" + ORIGINAL_OR, "");
		body = body.replaceAll("\\" + ORIGINAL_KEYWORD, "true");
		if (FSTGenComposerExtension.key) {
			body = "\r\n\t @ requires FeatureModel.fm();\r\n\t @ " + body;
		} else {
			body = "\r\n\t @ " + body;
		}
		
		if (!body.endsWith("\r\n\t")) {
			body = body + "\r\n\t";
		}
		terminal.setBody(body);
	}
}
