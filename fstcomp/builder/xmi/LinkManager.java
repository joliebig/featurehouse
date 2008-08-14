package builder.xmi;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class LinkManager {
	// Mapping of class ID and class Name needed for associations
	private Map<String, String> classMap = new HashMap<String, String>();
	// Mapping of enum ID
	private Map<String, String> enumMap = new HashMap<String, String>();
	// Mapping of 
	
	
	
	
	
	public void addClass(String id, String name) {
		classMap.put(id, name);
	}
	
	public String getClassName(String id) {
		return classMap.get(id);
	}
	
	public void addEnum(String id, String name) {
		enumMap.put(id, name);
	}
	
	public String getEnumName(String id) {
		return enumMap.get(id);
	}
	
	
}
