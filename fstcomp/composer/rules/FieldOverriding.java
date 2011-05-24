package composer.rules;

import java.util.LinkedHashSet;
import java.util.StringTokenizer;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class FieldOverriding {
	public final static String COMPOSITION_RULE_NAME = "FieldOverriding";
	public static void compose(FSTTerminal terminalA, FSTTerminal terminalB, FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {

		specializeModifiers(terminalA, terminalB);

		String bodyA = terminalA.getBody();
		String bodyB = terminalB.getBody();
		String compBody;

		
		if(!bodyB.contains(",") && !bodyA.contains(",")) {

			if(bodyB.contains("=")) {
				if(bodyA.contains("="))
					compBody = bodyA;
				else
					compBody = bodyB;
			} else {
				compBody = bodyA;
			}
			terminalComp.setBody(compBody);
		} else {
			System.err.println("Error: compound field declarations cannot be composed: \'" + bodyA + "\' and \'" + bodyB + "\'");
		}
	}
	
	private static void specializeModifiers(FSTTerminal terminalA, FSTTerminal terminalB) {

		if(terminalA.getBody().contains("@") || terminalB.getBody().contains("@"))
			return;
		
		//System.err.println("terminalA: " + terminalA.getBody());
		//System.err.println("terminalB: " + terminalB.getBody());
		
		StringTokenizer stA;
		if((terminalA.getBody().indexOf(",") > 0 && terminalA.getBody().indexOf("=") > 0 && terminalA.getBody().indexOf(",") < terminalA.getBody().indexOf("=")) || (terminalA.getBody().indexOf(",") > 0 && terminalA.getBody().indexOf("=") < 0)) {
			 stA = new StringTokenizer(terminalA.getBody(), ",");
		} else if((terminalA.getBody().indexOf(",") > 0 && terminalA.getBody().indexOf("=") > 0 && terminalA.getBody().indexOf("=") < terminalA.getBody().indexOf(",")) || (terminalA.getBody().indexOf(",") < 0 && terminalA.getBody().indexOf("=") > 0)) {
			 stA = new StringTokenizer(terminalA.getBody(), "=");
		} else {
			 stA = new StringTokenizer(terminalA.getBody(), ";");
		}

		StringTokenizer stB;
		if((terminalB.getBody().indexOf(",") > 0 && terminalB.getBody().indexOf("=") > 0  && terminalB.getBody().indexOf(",") < terminalB.getBody().indexOf("=")) || (terminalB.getBody().indexOf(",") > 0 && terminalB.getBody().indexOf("=") < 0)) {
			 stB = new StringTokenizer(terminalB.getBody(), ",");
		} else if((terminalB.getBody().indexOf(",") > 0 && terminalB.getBody().indexOf("=") > 0 && terminalB.getBody().indexOf("=") < terminalB.getBody().indexOf(",")) || (terminalB.getBody().indexOf(",") < 0 && terminalB.getBody().indexOf("=") > 0)) {
			 stB = new StringTokenizer(terminalB.getBody(), "=");
		} else {
			 stB = new StringTokenizer(terminalB.getBody(), ";");
		}

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

			if (!modifierArrayA[modifierArrayA.length - 2]
					.equals(modifierArrayB[modifierArrayB.length - 2])) {
				System.err.println("Warning: return types of the two fields `"
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
			boolean isFinal = false;
			boolean isStatic = false;

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
						removedDuplicates = removedDuplicates.replaceAll("final", "");
						removedDuplicates += modifier + " ";
					} else if (modifier.equals("final") && !isAbstract && !isFinal) {
						isFinal = true;
						removedDuplicates += modifier + " ";
					} else if (modifier.equals("static") && !isStatic) {
						isStatic = true;
						removedDuplicates += modifier + " ";
					} else if (!modifier.equals("abstract")
							&& !modifier.equals("final")
							&& !modifier.equals("static")) {
						removedDuplicates += modifier + " ";
					}
				}
			}

			//System.err.println("RM: " + removedDuplicates);
			
			String oldBodyA;
			if((terminalA.getBody().indexOf(",") > 0 && terminalA.getBody().indexOf("=") > 0 && terminalA.getBody().indexOf(",") < terminalA.getBody().indexOf("=")) || (terminalA.getBody().indexOf(",") > 0 && terminalA.getBody().indexOf("=") < 0)) {
				 oldBodyA = terminalA.getBody().substring(terminalA.getBody().indexOf(","));
			} else if((terminalA.getBody().indexOf(",") > 0 && terminalA.getBody().indexOf("=") > 0 && terminalA.getBody().indexOf("=") < terminalA.getBody().indexOf(",")) || (terminalA.getBody().indexOf(",") < 0 && terminalA.getBody().indexOf("=") > 0)) {
				oldBodyA = terminalA.getBody().substring(terminalA.getBody().indexOf("="));
			} else {
				oldBodyA = terminalA.getBody().substring(terminalA.getBody().indexOf(";"));
			}
			
			terminalA.setBody(removedDuplicates + " " + oldBodyA);

			String oldBodyB;
			if((terminalB.getBody().indexOf(",") > 0 && terminalB.getBody().indexOf("=") > 0  && terminalB.getBody().indexOf(",") < terminalB.getBody().indexOf("=")) || (terminalB.getBody().indexOf(",") > 0 && terminalB.getBody().indexOf("=") < 0)) {
				 oldBodyB = terminalB.getBody().substring(terminalB.getBody().indexOf(","));
			} else if((terminalB.getBody().indexOf(",") > 0 && terminalB.getBody().indexOf("=") > 0 && terminalB.getBody().indexOf("=") < terminalB.getBody().indexOf(",")) || (terminalB.getBody().indexOf(",") < 0 && terminalB.getBody().indexOf("=") > 0)) {
				oldBodyB = terminalB.getBody().substring(terminalB.getBody().indexOf("="));
			} else {
				oldBodyB = terminalB.getBody().substring(terminalB.getBody().indexOf(";"));
			}
			
			terminalB.setBody(removedDuplicates + " " + oldBodyB);
			
			//System.err.println("terminalC: " + terminalB.getBody());
		}
	}

}
