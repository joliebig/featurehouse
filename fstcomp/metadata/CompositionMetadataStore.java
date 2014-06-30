package metadata;

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

/**
 * 
 * @author Hendrik Speidel <speidel@fim.uni-passau.de>
 *
 * stores meta data about functions.
 * 
 * Stored info:
 * 
 *  -order that features are composed
 * 	-which feature introduced a particular function
 *  
 *
 */
public final class CompositionMetadataStore {
	
	private Map<String, Map<String, String>> mapping = new LinkedHashMap<String, Map<String, String>>();
	
	public String outputDir = "";
	
	private List<String> features = new ArrayList<String>();
	
	public void addFeature(String feature) {
		if (! features.contains(feature))
			features.add(feature);
	}
	
	
	/**
	 * adds a method/function mapping to the meta data
	 * 
	 * @param fnType
	 * @param feature
	 * @param rewrittenFnType
	 */
	public void putMapping(String fnType, String feature, String rewrittenFnType) {
		if (fnType.contains("__wrappee__")) {
			return;
		}
		Map<String, String> inner;
		if (!mapping.containsKey(fnType)) {
			inner = new LinkedHashMap<String, String>();
			mapping.put(fnType, inner);
		} else {
			//if (!rewrittenFnType.contains("__wrappee__")) {
			//	return;
			//}
			inner = mapping.get(fnType);
			
			for (Map.Entry<String, String> entry: inner.entrySet()) {
				if (!entry.getValue().contains("__wrappee__")) {
					entry.setValue(rewrittenFnType);
				}
			}
			
		}
		inner.put(feature, rewrittenFnType);		
	}

	/**
	 * 
	 * recursively searches the tree for `Func` nodes and records
	 * meta data for discovered ones.
	 * 
	 * @param n FSTNode to be searched for `Func` nodes
	 */
	public void discoverFuncIntroductions(FSTNode n) {
		if (n.getType().equals("Func")) {
			String funcName = getMethodName(n);
			putMapping(funcName, getFeatureName(n), funcName);
			return;
		}
		
		if (n instanceof FSTNonTerminal) {
			FSTNonTerminal nt = (FSTNonTerminal) n;
			for (FSTNode child: nt.getChildren()) {
				discoverFuncIntroductions(child);
			}			
		}
	}
	
	
	/**
	 * extracts the feature name
	 * 
	 * @param node
	 * 			`Feature` node or descendant of a `Feature` node.
	 * @return
	 * 		name of the feature this node is a descendant of, e.g. "Base".
	 */
	public String getFeatureName(FSTNode node) {
		if (node.getType().equals("Feature"))
			return node.getName();
		else
			return getFeatureName(node.getParent());
	}
	
	/**
	 * extracts the method name (function name for C) from the node
	 * 
	 * this assumes the node is a `Func` node.
	 * 
	 * @param node 
	 * 				a `Func` node.
	 * @return
	 * 				the method name e.g. "main"
	 */
	public String getMethodName(FSTNode node) {
		String methodName = node.getName();

		StringTokenizer st = new StringTokenizer(methodName, "(");
		if (st.hasMoreTokens()) {
			methodName = st.nextToken();
		}
		st = new StringTokenizer(methodName, " ");

		while (st.hasMoreTokens()) {
			methodName = st.nextToken();
		}
		
		return methodName;
	}
	
	/**
	 * 
	 * @return String representation of the meta data in JSON format.
	 * 			
	 */
	@Override
	public String toString() {
		StringBuffer buff = new StringBuffer();
		buff.append("{\n\t\"features\" : [");
		boolean first = true;
		for (String feature: features) {
			if (first) {
				first = false;
				buff.append("\n");
			} else {
				buff.append(",\n");
			}
			buff.append("\t\t\"" + feature + "\"");
		}
		
		buff.append("\n\t],\n\t\"roles\": ");
		buff.append("{");
		first = true;
		for (Map.Entry<String, Map<String, String>> entry: mapping.entrySet()) {
			if (first) {
				first = false;
				buff.append("\n");
			} else {
				buff.append(",\n");
			}
			buff.append("\t\t");
			buff.append("\"");
			buff.append(entry.getKey());
			buff.append("\": {");
			
			boolean innerFirst = true;
			
			for (Map.Entry<String, String> inner: entry.getValue().entrySet()) {
				if (innerFirst) {
					innerFirst = false;
					buff.append("\n");
				} else {
					buff.append(",\n");
				}
				buff.append("\t\t\t");
				buff.append("\"");
				buff.append(inner.getKey());
				buff.append("\": ");
				buff.append("\"");
				buff.append(inner.getValue());
				buff.append("\"");
			}
			
			buff.append("\n\t\t}");			
		}
		buff.append("\n\t}\n");
		buff.append("\n}\n");
		return buff.toString();
	}
	
	public void saveToFile(String filename) throws IOException {
		FileWriter fw = null;
		try {
			fw = new FileWriter(filename);
			fw.write(this.toString());
		} finally {
			if (fw != null) {
				fw.close();
			}
		}
	}
	
	private CompositionMetadataStore() {}	
	
	@Override
	protected final Object clone() throws CloneNotSupportedException {
		throw new CloneNotSupportedException();
	}
		
	private static CompositionMetadataStore instance = null;
	
	public static final synchronized CompositionMetadataStore getInstance() {
		if (instance == null) {
			instance = new CompositionMetadataStore();
		}
		return instance;
	}

	/**
	 * Returns a list of all feature names of the subject system. 
	 * Each name occurs only once in the list.
	 * We chose a List instead of a Set because the order of the features is important (same order as in the expression file).
	 */
	public List<String> getFeatures() {
		return new ArrayList<String>(features);
	}
	public void clearFeatures() {
		features.clear();
	}
}
