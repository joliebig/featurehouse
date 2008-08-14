package printer.xmi;

import java.util.LinkedList;
import java.util.List;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class Association extends XMINode {
	
	private List<AssociationEnd> associationEnds = new LinkedList<AssociationEnd>();
	private Element classNode;

	/**
	 * 
	 * @param rootNode
	 */
	public Association(FSTNode rootNode) {
		super();
		initAssociation(rootNode);
	}
	
	public Association(FSTNode rootNode, Element classNode) {
		super();
		initAssociation(rootNode);
		this.classNode = classNode;
	}
	
	private void initAssociation(FSTNode rootNode) {
		FSTNonTerminal assocItems = (FSTNonTerminal) rootNode;
		super.setAttribute("name", assocItems.getName());

		for (FSTNode assocItem : assocItems.getChildren()) {
			FSTNonTerminal assocItemNT = (FSTNonTerminal) assocItem;
			
			String type = assocItemNT.getType();
			if (type.equals("XMIAssociationDetails")) {
				for (FSTNode assocEndItems : assocItemNT.getChildren()) {
					FSTTerminal assocEndItem = (FSTTerminal) assocEndItems;
					String name = assocEndItem.getType();
					String value = assocEndItem.getName();
					super.setAttribute(name, value);
				}
			} else {
				AssociationEnd assocEnd = new AssociationEnd(assocItem);
				associationEnds.add(assocEnd);
			}
		}
	}
	
	
	
	/**
	 * 
	 * @param parent
	 * @param doc
	 */
	public void createXMI(Node parent, Document doc, LinkManager linkManager) {
		
		Element assoc = doc.createElement(Strings.ASSOC);
		Element assocConn = doc.createElement(Strings.ASSOCCONN);
		
		assoc.setAttribute(Strings.NAME, getAttribute(Strings.NAME));
		assoc.setAttribute(Strings.ISABSTRACT, getAttribute(Strings.ISABSTRACT));
		assoc.setAttribute(Strings.ISROOT, getAttribute(Strings.ISROOT));
		assoc.setAttribute(Strings.ISLEAF, getAttribute(Strings.ISLEAF));
		assoc.setAttribute(Strings.ISSPEC, getAttribute(Strings.ISSPEC));
		
		assoc.appendChild(assocConn);
		
		for (AssociationEnd associationEnd : associationEnds) {
			associationEnd.createXMI(assocConn, doc, linkManager);
		}
		if (classNode == null) {
			parent.appendChild(assoc);
		} else {
			classNode.setAttribute(Strings.NAME, getAttribute(Strings.NAME));
			classNode.setAttribute(Strings.ISABSTRACT, getAttribute(Strings.ISABSTRACT));
			classNode.setAttribute(Strings.ISROOT, getAttribute(Strings.ISROOT));
			classNode.setAttribute(Strings.ISLEAF, getAttribute(Strings.ISLEAF));
			classNode.setAttribute(Strings.ISSPEC, getAttribute(Strings.ISSPEC));
			classNode.appendChild(assocConn);
		}	
	}
	
	
	/**
	 * 
	 * @param linkManager
	 */
	/*
	public void createLinks(LinkManager linkManager) {
		for (AssociationEnd assocEnd : associationEnds) {
			String oldRef = assocEnd.getAttribute(Strings.IDREF);
			String newRef = linkManager.getClassLink(oldRef);
			assocEnd.setAttribute(Strings.IDREF, newRef);
		}
	}*/
	
	
	/**
	 * 
	 *
	 */
	class AssociationEnd extends XMINode {
		/**
		 * 
		 * @param rootNode
		 */
		AssociationEnd(FSTNode rootNode) {
			super();
			FSTNonTerminal node = (FSTNonTerminal)rootNode;
			for (FSTNode nodeNT : node.getChildren()) {
				FSTTerminal assocItem = (FSTTerminal) nodeNT;
				String name = assocItem.getType();
				String value = assocItem.getName();
				super.setAttribute(name, value);
				
			}
		}
		
		
		/**
		 * 
		 * @param rootNode
		 * @param doc
		 */
		void createXMI(Node rootNode, Document doc, LinkManager linkManager) {
			//root
			Element assocEnd = doc.createElement(Strings.ASSOCEND);
			assocEnd.setAttribute(Strings.VISIBILITY, getAttribute(Strings.VISIBILITY));
			assocEnd.setAttribute(Strings.ISSPEC, getAttribute(Strings.ISSPEC));
			assocEnd.setAttribute(Strings.ISNAVI, getAttribute(Strings.ISNAVI));
			assocEnd.setAttribute(Strings.ORDERING, getAttribute(Strings.ORDERING));
			assocEnd.setAttribute(Strings.AGGREGATION, getAttribute(Strings.AGGREGATION));
			assocEnd.setAttribute(Strings.SCOPE, getAttribute(Strings.SCOPE));
			assocEnd.setAttribute(Strings.CHANGEABILITY, getAttribute(Strings.CHANGEABILITY));
			
			//child node multiplicity
			Element assocEndMult = doc.createElement(Strings.ASSOCENDMULT);
			Element mult = doc.createElement(Strings.MULT);
			Element multRange = doc.createElement(Strings.MULTRANGE);
			Element multRangeItem = doc.createElement(Strings.MULTRANGEITEM);
			multRangeItem.setAttribute(Strings.LOWER, getAttribute(Strings.LOWER));
			multRangeItem.setAttribute(Strings.UPPER, getAttribute(Strings.UPPER));
			multRange.appendChild(multRangeItem);
			mult.appendChild(multRange);
			assocEndMult.appendChild(mult);
			
			//child node participant
			Element assocEndParticipant = doc.createElement(Strings.ASSOCENDPARTICIPANT);
			Element assocEndClass = doc.createElement(Strings.CLASS);
			assocEndClass.setAttribute(Strings.IDREF, getAttribute(Strings.IDREF));
			linkManager.addClassLink(assocEndClass);
			assocEndParticipant.appendChild(assocEndClass);
			assocEnd.appendChild(assocEndParticipant);
			assocEnd.appendChild(assocEndMult);
			
			rootNode.appendChild(assocEnd);
		}
	}
}
