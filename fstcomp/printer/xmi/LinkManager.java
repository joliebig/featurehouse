package printer.xmi;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Element;


public class LinkManager {

	//DataTypes
	private List<DataType> dataTypes = new LinkedList<DataType>();
	//Classes
	private List<Element> classLinks = new LinkedList<Element>();
	private Map<String, String> classMap = new HashMap<String, String>();
	//Enums
	private List<Element> enumLinks = new LinkedList<Element>();
	private Map<String, String> enumMap = new HashMap<String, String>();
	
	
	/**
	 * 
	 * @param name
	 * @param idRef
	 */
	public void addClass(String name, String idRef) {
		classMap.put(name, idRef);
	}
	
	
	/**
	 * 
	 * @param node
	 */
	public void addClassLink(Element node) {
		classLinks.add(node);
	}
		
	
	/**
	 * 
	 * @param parent
	 * @param document
	 */
	public void createClassLinks() {
		for (Element node : classLinks) {
			String idRef = node.getAttribute(Strings.IDREF);
			String newRef = classMap.get(idRef);
			node.setAttribute(Strings.IDREF, newRef);
		}
	}
	
	/**
	 * 
	 * @param name
	 * @param idRef
	 */
	public void addEnum(String name, String idRef) {
		enumMap.put(name, idRef);
	}
	
	
	/**
	 * 
	 * @param node
	 */
	public void addEnumLink(Element node) {
		enumLinks.add(node);
	}
		
	
	/**
	 * 
	 * @param parent
	 * @param document
	 */
	public void createEnumLinks() {
		for (Element node : enumLinks) {
			String idRef = node.getAttribute(Strings.IDREF);
			String newRef = enumMap.get(idRef);
			node.setAttribute(Strings.IDREF, newRef);
		}
	}
	
	
	/**
	 * 
	 * @param dataType
	 * @return
	 */
	public String addDataType(DataType dataType) {
		//Is DataType already exisiting?
		for (DataType refType : dataTypes) {
			if (refType.equals(dataType)) {
				//return refType.getID();
				return refType.getAttribute(Strings.ID);
			}
		}
		//Insert new DataType
		dataTypes.add(dataType);
		//return dataType.getID();
		return dataType.getAttribute(Strings.ID);
	}
	
	
	/**
	 * 
	 * @param parent
	 * @param document
	 */
	public void createDataTypes(Node parent, Document document) {
		for (DataType dataType : dataTypes) {
			Element dataNode = document.createElement(Strings.TYPE);
			dataNode.setAttribute(Strings.ID, dataType.getAttribute(Strings.ID));
			dataNode.setAttribute(Strings.NAME, dataType.getAttribute(Strings.NAME));
			dataNode.setAttribute(Strings.ISSPEC, dataType.getAttribute(Strings.ISSPEC));
			dataNode.setAttribute(Strings.ISROOT, dataType.getAttribute(Strings.ISROOT));
			dataNode.setAttribute(Strings.ISLEAF, dataType.getAttribute(Strings.ISLEAF));
			dataNode.setAttribute(Strings.ISABSTRACT, dataType.getAttribute(Strings.ISABSTRACT));
			parent.appendChild(dataNode);
		}
	}
	
}
