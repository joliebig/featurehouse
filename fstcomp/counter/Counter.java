package counter;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
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
			return "Introduces " + feature + " " + name + " " + type;
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
			
			collectJava(node);
			
		} else if(node.getType().equals("Func") ||
				node.getType().equals("StmtYL") ||
				node.getType().equals("StmtTL")) {
			collectC(node);
		}
		if(node instanceof FSTNonTerminal)
		for(FSTNode child : ((FSTNonTerminal)node).getChildren())
			collect(child);
    }

	private void collectJava(FSTNode node) {
		String name = node.getName();
		String type = node.getType();
		String feature = getFeatureName(node);

		if(node.getType().equals("InnerClassDecl")) {
			type = "ClassDeclaration";
		}
		
		if(node.getType().equals("MethodDecl") || node.getType().equals("ConstructorDecl")) {
			//name = node.getName().replace("{FormalParametersInternal}", "");
			name = name.substring(0, name.indexOf("(")) + "()";
			/*StringTokenizer st = new StringTokenizer(name, "-");
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
			}*/
		} 
		
		name = getQualifiedJavaName(name, node.getParent()); 
		
		Entry entry = new Entry(name, type, feature);
		data.add(entry);
		//System.err.println(entry);
	}
	
	private void collectC(FSTNode node) {
		String name = node.getName();
		String type = node.getType();
		String feature = getFeatureName(node);

		if(node.getType().equals("Func")) {
			name = name.substring(0, name.indexOf("(")) + "()";
		} 
		
		name = getQualifiedCName(name, node.getParent()); 
		
		Entry entry = new Entry(name, type, feature);
		data.add(entry);
	}
	
	public void writeFile(File file) {
		try {
			//System.err.println(file.getAbsolutePath());
			file.createNewFile();
			BufferedWriter textFileWriter = new BufferedWriter(new FileWriter(file));
			for(Entry entry : data)
				textFileWriter.write(entry.toString() + "\n");
			textFileWriter.flush();
			textFileWriter.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}
	
    private static String getQualifiedJavaName(String name, FSTNode node) {
    	if (node.getType().equals("Feature")) {
    	    return name;
    	} else {
    		if(node.getType().equals("ClassDeclaration") || node.getType().equals("InnerClassDecl")) {
    			if(name.contains(".")) {
    				name = node.getName() + "::" + name;
    			} else {
    				name = node.getName() + "." + name;
    			}
    		} else if(node.getType().equals("Folder")) {
    			name = node.getName() + "::" + name;
    		}
    		
    	    return getQualifiedJavaName(name, node.getParent());
        }
		
	}
    private static String getQualifiedCName(String name, FSTNode node) {
    	if (node.getType().equals("Feature")) {
    	    return name;
    	} else {
    		if(node.getType().equals("H-File") || node.getType().equals("C-File") || node.getType().equals("StructDec")) {
   				name = node.getName() + "." + name;
    		} else if(node.getType().equals("Folder")) {
    			name = node.getName() + "/" + name;
    		}
    		
    	    return getQualifiedCName(name, node.getParent());
        }
		
	}
	private static String getFeatureName(FSTNode node) {
    	if (node.getType().equals("Feature"))
    	    return node.getName();
    	else
    	    return getFeatureName(node.getParent());
    }
	
}
