package composer.rules;

import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.StringTokenizer;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class JavaMethodOverriding {
	public final static String COMPOSITION_RULE_NAME = "JavaMethodOverriding";

	public static void compose(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {

		specializeModifiers(terminalA, terminalB);

		if (!terminalA.getBody().matches("(?s).*\\s*original\\s*.*")) {
			terminalComp.setBody(terminalA.getBody());
		} else {
			FSTTerminal terminalComp2 = (FSTTerminal) terminalB.getDeepClone();
			nonterminalParent.addChild(terminalComp2);

			String oldMethodName = terminalB.getName();

			StringTokenizer st = new StringTokenizer(oldMethodName, "(");
			if (st.hasMoreTokens()) {
				oldMethodName = st.nextToken();
			}
			st = new StringTokenizer(oldMethodName, " ");

			while (st.hasMoreTokens()) {
				oldMethodName = st.nextToken();
			}
			

			String toReplace = "original\\s*\\(";
			String newMethodName = oldMethodName + "__wrappee__" + getFeatureName(terminalB);
			String newBody = terminalComp.getBody().replaceAll(toReplace, newMethodName + "(");
			terminalComp.setBody(newBody);

			String auxBody = "";
			st = new StringTokenizer(terminalComp2.getBody(), "(");
			if (st.hasMoreTokens()) {
				auxBody = st.nextToken();
			}

			//System.err.println(oldMethodName);
			//System.err.println(auxBody);
			
			//String sep = "";
			
			st = new StringTokenizer(auxBody);
			String prefix = "";
			boolean found = false;
			while (st.hasMoreTokens() && !found) {
				String token = st.nextToken();
				if (oldMethodName.equals(token)) {
					found = true;
				} else {
					prefix += token + " ";
				}
			}

			// Replace '[]' for regex
			String modPrefix = "";
			for (char c : prefix.toCharArray()) {
				if (c == '[')
					modPrefix += "\\[";
				else if (c == ']')
					modPrefix += "\\]";
				else if (c == '*')
					modPrefix += "\\*";
				else
					modPrefix += String.valueOf(c);
			}

			modPrefix = modPrefix.trim();

			//System.err.println("-------" + prefix);
			//System.err.println("#######" + terminalComp2.getBody());
			//System.err.println("+++++++" + terminalComp2.getBody().replaceFirst(modPrefix, ""));
			
			prefix = prefix.replaceFirst("public", "private");
			prefix = prefix.replaceFirst("protected", "private");
			if(!prefix.contains("private") && !isC(nonterminalParent))
				prefix = "private " + prefix;
			
			terminalComp2.setBody(prefix + terminalComp2.getBody().replaceFirst(modPrefix, "").replaceFirst(oldMethodName, newMethodName));
			terminalComp2.setName(newMethodName);
		}
	}

	private static String getFeatureName(FSTNode node) {
		if (node.getType().equals("Feature"))
			return node.getName();
		else
			return getFeatureName(node.getParent());
	}

	private static boolean isC(FSTNode node) {
		if (node.getType().equals("C-File")) {
			return true;
		} else {
			FSTNode parent = node.getParent();
			if(parent != null) {
				return isC(parent);
			} else {
				return false;
			}
		}
			
	}

	
	private static void specializeModifiers(FSTTerminal terminalA, FSTTerminal terminalB) {

		if(terminalA.getBody().contains("@") || terminalB.getBody().contains("@"))
			return;
		
		StringTokenizer stA = new StringTokenizer(terminalA.getBody(), "(");
		StringTokenizer stB = new StringTokenizer(terminalB.getBody(), "(");

		if (stA.hasMoreTokens() && stB.hasMoreTokens()) {
			stA = new StringTokenizer(stA.nextToken(), " ");
			LinkedHashSet<String> modifierSetA = new LinkedHashSet<String>();
			while (stA.hasMoreTokens()) {
				modifierSetA.add(stA.nextToken());
			}
			stB = new StringTokenizer(stB.nextToken(), " ");
			LinkedHashSet<String> modifierSetB = new LinkedHashSet<String>();
			while (stB.hasMoreTokens()) {
				modifierSetB.add(stB.nextToken());
			}

			String[] modifierArrayA = new String[modifierSetA.size()];
			modifierSetA.toArray(modifierArrayA);

			String[] modifierArrayB = new String[modifierSetB.size()];
			modifierSetB.toArray(modifierArrayB);

			if(modifierArrayA.length <=1 || modifierArrayB.length <= 1) 
				return;
			
			if (!modifierArrayA[modifierArrayA.length - 2]
					.equals(modifierArrayB[modifierArrayB.length - 2])) {
				System.err.println("Warning: return types of the two methods `"
						+ modifierArrayA[modifierArrayA.length - 1]
						+ "' are not compatible: "
						+ modifierArrayA[modifierArrayA.length - 2] + " != "
						+ modifierArrayB[modifierArrayB.length - 2]);
			}

			String removedDuplicates = new String();
			String[] modifierArrayRes = new String[modifierArrayA.length + modifierArrayB.length - 2];
			System.arraycopy(modifierArrayB, 0, modifierArrayRes, 0, modifierArrayB.length - 2);
			System.arraycopy(modifierArrayA, 0, modifierArrayRes, modifierArrayB.length - 2, modifierArrayA.length);

			boolean isPublic = false;
			boolean isProtected = false;
			boolean isPrivate = false;
			boolean isAbstract = false;
			LinkedList<String> otherModifiers = new LinkedList<String>();
			
			for (int i = 0; i < modifierArrayRes.length; i++) {
				String modifier = modifierArrayRes[i].trim();
				if (modifier.equals("private") && !isPublic && !isProtected && !isPrivate) {
					isPrivate = true;
					removedDuplicates += modifier + " ";
				} else if (modifier.equals("protected") && !isPublic && !isProtected ) {
					isProtected = true;
					removedDuplicates = removedDuplicates.replaceAll("private",
							"");
					removedDuplicates += modifier + " ";
				} else if (modifier.equals("public") && !isPublic) {
					isPublic = true;
					removedDuplicates = removedDuplicates.replaceAll("private",
							"");
					removedDuplicates = removedDuplicates.replaceAll(
							"protected", "");
					removedDuplicates += modifier + " ";
				} else if (!modifier.equals("public")
						&& !modifier.equals("protected")
						&& !modifier.equals("private")) {
					if (modifier.equals("abstract")) {
						isAbstract = true;
						removedDuplicates = removedDuplicates.replaceAll(
								"final", "");
						removedDuplicates += modifier + " ";
					} else if (modifier.equals("final") && !isAbstract) {
						removedDuplicates += modifier + " ";
					} else if (!modifier.equals("abstract")
							&& !modifier.equals("final")) {
						
						boolean in = false;
						for(String otherModifier : otherModifiers) {
							if(otherModifier.equals(modifier))
								in = true;
						}
						if(!in) {
							removedDuplicates += modifier + " ";
							otherModifiers.add(modifier);
						}
					}
				}
			}

			if(terminalA.getBody().contains("("))
				terminalA.setBody(removedDuplicates + " " + terminalA.getBody().substring(terminalA.getBody().indexOf("(")));
			
			if(terminalB.getBody().contains("("))
				terminalB.setBody(removedDuplicates + " " + terminalB.getBody().substring(terminalB.getBody().indexOf("(")));
			
		}
	}
}
