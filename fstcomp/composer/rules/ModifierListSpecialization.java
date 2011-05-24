package composer.rules;

import java.util.LinkedHashSet;
import java.util.StringTokenizer;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class ModifierListSpecialization {
	public final static String COMPOSITION_RULE_NAME = "ModifierListSpecialization";
	public static void compose(FSTTerminal terminalA, FSTTerminal terminalB, FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {

		if(terminalA.getBody().contains("@") || terminalB.getBody().contains("@")) {
		
		} else {
			if(terminalA.getBody().length() == 0) {
				terminalComp.setBody(terminalB.getBody());
				return;
			}
			if(terminalB.getBody().length() == 0) {
				terminalComp.setBody(terminalA.getBody());
				return;
			}

			StringTokenizer st = new StringTokenizer(terminalB.getBody() + " " + terminalA.getBody());
		
			LinkedHashSet<String> modifierSet = new LinkedHashSet<String>(); 
			while (st.hasMoreTokens()) {
				modifierSet.add(st.nextToken());
			}
			String removedDuplicates = new String();
			String[] modifierArray = new String[modifierSet.size()];
			modifierSet.toArray(modifierArray);
			boolean isPublic = false;
			boolean isProtected = false;
			boolean isAbstract = false;

   
			for(int i = 0; i < modifierArray.length; i++) {
				String modifier = modifierArray[i].trim(); 
				if(modifier.equals("private") && !isPublic && !isProtected) {
					removedDuplicates += modifier + " ";
				} else if(modifier.equals("protected") && !isPublic) {
					isProtected = true;
					removedDuplicates = removedDuplicates.replaceAll("private", "");
					removedDuplicates += modifier + " ";
				} else if(modifier.equals("public")) {
					isPublic = true;
					removedDuplicates = removedDuplicates.replaceAll("private", "");
					removedDuplicates = removedDuplicates.replaceAll("protected", "");
					removedDuplicates += modifier + " ";
				} else if(!modifier.equals("public") && !modifier.equals("protected") && !modifier.equals("private")) {
					if(modifier.equals("abstract")) {
						isAbstract = true;
						removedDuplicates = removedDuplicates.replaceAll("final", "");
						removedDuplicates += modifier + " ";
					} else if(modifier.equals("final") && !isAbstract) {
						removedDuplicates += modifier + " ";
					} else if(!modifier.equals("abstract") && !modifier.equals("final")) {
						removedDuplicates += modifier + " ";
					}
				}
			}
			terminalComp.setBody(removedDuplicates);

		}
	}
}
