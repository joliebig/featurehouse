package printer.xmi;

import java.util.HashMap;
import java.util.Map;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class XMINode {
	/*private String name;
	private String id;
	private String isSpecification;
	private String isRoot;
	private String isLeaf;
	private String isAbstract;*/
	
	private Map<String, String> attributeMap = new HashMap<String,String>();
	
	public XMINode() {}
	/*
	public XMINode(String id, String name, String isSpecification, String isRoot, String isLeaf, String isAbstract) {
		this.id = id;
		this.name = name;
		this.isSpecification = isSpecification;
		this.isRoot = isRoot;
		this.isLeaf = isLeaf;
		this.isAbstract = isAbstract;
	}*/
	
	public XMINode(FSTNonTerminal node, String id) {
		//this.id = id;
		setAttribute(Strings.ID, id);
		//name = node.getName();
		setAttribute(Strings.NAME, node.getName());
		for (FSTNode dataTypeAttribute : node.getChildren()) {
			String type = dataTypeAttribute.getType();
			String nodeName = dataTypeAttribute.getName();
			
			if (type.equals("name") && getAttribute(Strings.NAME).equals("")) {
				setAttribute(Strings.NAME, node.getName());
			}
			
			setAttribute(type, nodeName);
			
			/*
			if (type.equals(Strings.ISSPEC)) {
				isSpecification = nodeName;
			} else if (type.equals(Strings.ISROOT)) {
				isRoot = nodeName;
			} else if (type.equals(Strings.ISLEAF)) {
				isLeaf = nodeName;
			} else if (type.equals(Strings.ISABSTRACT)) {
				isAbstract = nodeName;
			}*/
		}
	}
	
	public void setAttribute(String name, String value) {
		attributeMap.put(name, value);
	}
	
	public String getAttribute(String name) {
		return attributeMap.get(name);
	}
	/*
	public String getName() {
		return name;
	}
	
	public String getIsSpecification() {
		return isSpecification;
	}
	
	public String getIsRoot() {
		return isRoot;
	}
	
	public String getIsLeaf() {
		return isLeaf;
	}
	
	public String getIsAbstract() {
		return isAbstract;
	}
	
	public String getID() {
		return id;
	}
	
	public void setID(String id) {
		this.id = id;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public void setIsSpecification(String isSpecification) {
		this.isSpecification = isSpecification;
	}
	
	public void setIsRoot(String isRoot) {
		this.isRoot = isRoot;
	}
	
	public void setIsLeaf(String isLeaf) {
		this.isLeaf = isLeaf;
	}
	
	public void setIsAbstract(String isAbstract) {
		this.isAbstract = isAbstract;
	}*/
	
	@Override
	public boolean equals(Object o) {
		if (o == null) {
			return false;
		} else {
			if (o instanceof DataType) {
				DataType ref = (DataType)o;
				if (ref.getAttribute(Strings.NAME).equals(getAttribute(Strings.NAME)) &&
					ref.getAttribute(Strings.ISABSTRACT).equals(getAttribute(Strings.ISABSTRACT)) &&
					ref.getAttribute(Strings.ISLEAF).equals(getAttribute(Strings.ISLEAF)) &&
					ref.getAttribute(Strings.ISROOT).equals(getAttribute(Strings.ISROOT)) &&
					ref.getAttribute(Strings.ISSPEC).equals(getAttribute(Strings.ISSPEC))) {
					return true;
				}
			}
			return false;
		}
	}
	
}
