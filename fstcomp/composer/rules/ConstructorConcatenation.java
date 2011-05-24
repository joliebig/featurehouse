package composer.rules;

import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.StringTokenizer;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class ConstructorConcatenation {
	public final static String COMPOSITION_RULE_NAME = "ConstructorConcatenation";
	public static void compose(FSTTerminal terminalA, FSTTerminal terminalB, FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
		
		specializeModifiers(terminalA, terminalB);
		
		String constructorA = terminalA.getBody();
		String constructorB = terminalB.getBody();
		constructorB = constructorB.substring(0, constructorB.lastIndexOf("}"));
		constructorA = constructorA.substring(constructorA.indexOf("{") + 1, constructorA.length());
		
		terminalComp.setBody(constructorB + constructorA);
	}
	
	private static void specializeModifiers(FSTTerminal terminalA, FSTTerminal terminalB) {

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

			String removedDuplicates = new String();
			String[] modifierArrayRes = new String[modifierArrayA.length + modifierArrayB.length - 1];
			System.arraycopy(modifierArrayB, 0, modifierArrayRes, 0, modifierArrayB.length - 1);
			System.arraycopy(modifierArrayA, 0, modifierArrayRes, modifierArrayB.length - 1, modifierArrayA.length);

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

			terminalA.setBody(removedDuplicates + " " + terminalA.getBody().substring(terminalA.getBody().indexOf("(")));
			terminalB.setBody(removedDuplicates + " " + terminalB.getBody().substring(terminalB.getBody().indexOf("(")));
			
		}
	}
}
