package counter;

import java.util.LinkedList;
import java.util.StringTokenizer;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class Counter {
	private class Entry {
		private String name = "";
		private String type = "";
		private String feature = "";
		public Entry(String n, String t, String f) {
			name = n; type = t; feature = f;
		}
		public String toString() {
			return "(" + name + " : " + type + ", " + feature + ")";
		}
	}
	LinkedList<Entry> data = new LinkedList<Entry>();
	public void collect(FSTNode node) {
	
		if(node.getType().equals("ClassDeclaration") ||
				node.getType().equals("MethodDecl") ||
				node.getType().equals("InnerEnumDecl") ||
				node.getType().equals("InnerClassDecl") ||
				node.getType().equals("FieldDecl") ||
				node.getType().equals("ConstructorDecl")) {
			
			String name = node.getName();
			String type = node.getType();
			String feature = getFeatureName(node);

			if(node.getType().equals("InnerClassDecl")) {
				type = "ClassDeclaration";
			}
			
			if(node.getType().equals("MethodDecl") || node.getType().equals("ConstructorDecl")) {
				name = node.getName().replace("{FormalParametersInternal}", "");
				StringTokenizer st = new StringTokenizer(name, "-");
				boolean even = true;
				name = st.nextToken();
				while(st.hasMoreTokens()) {
					if(even) {
						name += "-" + st.nextToken();
						even = false;
					} else {
						st.nextToken();
						even = true;
					}
				}
			} 
			
			Entry entry = new Entry(name, type, feature);
			data.add(entry);
			System.err.println(entry);
			
		}
		if(node instanceof FSTNonTerminal)
		for(FSTNode child : ((FSTNonTerminal)node).getChildren())
			collect(child);
    }
    private static String getFeatureName(FSTNode node) {
    	if (node.getType().equals("Feature"))
    	    return node.getName();
    	else
    	    return getFeatureName(node.getParent());
        }
}
