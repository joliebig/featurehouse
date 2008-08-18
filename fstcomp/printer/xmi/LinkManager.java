package printer.xmi;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Element;

/**
 * The LinkManager saves mappings between the names and ids of data types,
 *  classes and enumerations.
 */
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
	 * Add a new class mapping
	 * @param name name of the class
	 * @param idRef id of the class
	 */
	public void addClass(String name, String idRef) {
		classMap.put(name, idRef);
	}
	
	
	/**
	 * add a link to a class which reference has to updated
	 * @param node 
	 */
	public void addClassLink(Element node) {
		classLinks.add(node);
	}
		
	
	/**
	 * creates a consistent links between the name of an attribute
	 * or operation  to its referencing class
	 */
	public void createClassLinks() {
		for (Element node : classLinks) {
			String idRef = node.getAttribute(Strings.IDREF);
			String newRef = classMap.get(idRef);
			node.setAttribute(Strings.IDREF, newRef);
		}
	}
	
	/**
	 * add a link to an enumeration which reference has to updated
	 * @param name
	 * @param idRef
	 */
	public void addEnum(String name, String idRef) {
		enumMap.put(name, idRef);
	}
	
	
	/**
	 * add a link to an enumeration which reference has to updated
	 * @param node
	 */
	public void addEnumLink(Element node) {
		enumLinks.add(node);
	}
		
	
	/**
	 * creates a consistent links between the name of an attribute
	 * or operation  to its referencing enumeration
	 */
	public void createEnumLinks() {
		for (Element node : enumLinks) {
			String idRef = node.getAttribute(Strings.IDREF);
			String newRef = enumMap.get(idRef);
			node.setAttribute(Strings.IDREF, newRef);
		}
	}
	
	/**
	 * adds a new data type an checks whether another
	 * data type with the same attributes is 
	 * already existing
	 * @param dataType data type to be added
	 * @return unique ID of the data type
	 */
	public String addDataType(DataType dataType) {
		//Is DataType already exisiting?
		for (DataType refType : dataTypes) {
			if (refType.equals(dataType)) {
				return refType.getAttribute(Strings.ID);
			}
		}
		//No! Insert new DataType
		dataTypes.add(dataType);
		return dataType.getAttribute(Strings.ID);
	}
	
	
	/**
	 * add the stored data types to the corresponding xmi parent node
	 * @param parent xmi parent node
	 * @param document xmi document
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
