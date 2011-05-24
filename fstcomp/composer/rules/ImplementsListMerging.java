package composer.rules;

import java.util.LinkedHashSet;
import java.util.StringTokenizer;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class ImplementsListMerging {
	public final static String COMPOSITION_RULE_NAME = "ImplementsListMerging";
	public static void compose(FSTTerminal terminalA, FSTTerminal terminalB, FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
		String interfaceListA = terminalA.getBody().replaceFirst("implements", ", ");
		String interfaceListB = terminalB.getBody().replaceFirst("implements", ", ");
		String concatenatedList = interfaceListB + interfaceListA;
		concatenatedList = concatenatedList.replaceAll("\\s*", "");
		StringTokenizer st = new StringTokenizer(concatenatedList, ",");
		LinkedHashSet<String> interfaceSet = new LinkedHashSet<String>(); 
	    while (st.hasMoreTokens()) {
	    	interfaceSet.add(st.nextToken());
	    }
	    String removedDuplicates = new String();
	    String[] interfaceArray = new String[interfaceSet.size()];
	    interfaceSet.toArray(interfaceArray);
		for(int i = 0; i < interfaceArray.length - 1; i++)
			removedDuplicates += interfaceArray[i] + ", ";
		removedDuplicates += interfaceArray[interfaceArray.length - 1];
		terminalComp.setBody("implements " + removedDuplicates);
	}
}
