package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * Represents a state chart transition
 */
public class XMITransition extends XMINonTerminal {
	
	public XMITransition(Element node, Element root) {
		super("XMI:Transition", node.getAttribute("name"), node, root);
		if (node.getAttribute("name").equals("")) {
			System.err.println("Found a transition without name!");
		}
		setNodeAttribute("isSpecification", node.getAttribute("isSpecification"));
		setNodeAttribute("xmi.id", node.getNodeName() + node.getAttribute("name"));
		setNodeAttribute("name", node.getAttribute("name"));
		
		//Transition source
		NodeList transSources = node.getElementsByTagName("UML:Transition.source");
		Node transSource = transSources.item(0);
		Element transType = (Element) transSource.getChildNodes().item(1);
		addChild(new TransitionEnd(transType, "UML:Transition.source"));
		
		//Transition target
		NodeList transTargets = node.getElementsByTagName("UML:Transition.target");
		Node transTarget = transTargets.item(0);
		transType = (Element) transTarget.getChildNodes().item(1);
		addChild(new TransitionEnd(transType, "UML:Transition.target"));
	
		//Transition trigger
		NodeList transTriggers = node.getElementsByTagName("UML:Transition.trigger");
		if (transTriggers.getLength() > 0) {
			Element transTrigger = (Element) transTriggers.item(0);
			NodeList signalEvents = transTrigger.getElementsByTagName("UML:SignalEvent");
			Element signalEvent = (Element) signalEvents.item(0);
			setNodeAttribute("signalevent", signalEvent.getAttribute("xmi.idref"));
		}
		
		//Tranisition guard
		NodeList transGuards = node.getElementsByTagName("UML:Transition.guard");
		if (transGuards.getLength() > 0) {
			Element transGuard = (Element) transGuards.item(0);
			addChild(new TransitionFeature("UML:Transition.Guard", "guard", transGuard));
		}
		
		//Transition effect
		NodeList transEffects = node.getElementsByTagName("UML:Transition.effect");
		if (transEffects.getLength() > 0) {
			Element transEffect = (Element) transEffects.item(0);
			addChild(new TransitionFeature("UML:Transition.Effect", "effect", transEffect));
		}
	}
	
	

	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement("UML:Transition");
		Element transTrigger = doc.createElement("UML:Transition.trigger");
		Element signalEvent = doc.createElement("UML:SignalEvent");
		node.setAttribute("xmi.id", getNodeAttribute("xmi.id"));
		node.setAttribute("name", getNodeAttribute("name"));
		node.setAttribute("isSpecification", getNodeAttribute("isSpecification"));
		
		//add transition source, target as children
		for (FSTNode fstNode : getChildren()) {
			XMINode xmiNode = (XMINode) fstNode;
			node.appendChild(xmiNode.toXMI(doc));
		}
		
		//add signal event as child
		if (getNodeAttribute("signalevent") != null) {
			transTrigger.appendChild(signalEvent);
			signalEvent.setAttribute("xmi.idref", getNodeAttribute("signalevent"));
			node.appendChild(transTrigger);
		}
		
		return node;
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new XMITransition(getNode(), getRoot());
	}

	@Override
	public FSTNode getDeepClone() {
		XMITransition clone = new XMITransition(getNode(), getRoot());
		return clone;
	}
	
	/**
	 * A TransitionEnd contains a reference to a state. The name of a TransitionEnd
	 * has to be the same as the type of the referencing state.
	 */
	private class TransitionEnd extends XMITerminal {
		
		String type;
		TransitionEnd(Element node, String type) {
			super(node.getNodeName(), node.getAttribute("name"));
			this.type = type;
			String newID = IdToElement(node.getAttribute("xmi.idref"), node.getNodeName());
			if (newID.equals("")) {
				newID = node.getAttribute("xmi.idref");
			}
			setNodeAttribute("xmi.idref", node.getNodeName() + newID);
		}

		@Override
		public Element toXMI(Document doc) {
			Element node = doc.createElement(type);
			Element subNode = doc.createElement(getType());
			subNode.setAttribute("xmi.idref", getNodeAttribute("xmi.idref"));
			node.appendChild(subNode);
			return node;
		}
		
		@Override
		public FSTNode getShallowClone() {
			return new TransitionEnd(getNode(), type);
		}

		@Override
		public FSTNode getDeepClone() {
			return new TransitionEnd(getNode(), type);
		}
	}
	
	/**
	 * A TransitionFeature can either be a TransitionGuard or
	 * a TransitioEffect.
	 */
	private class TransitionFeature extends XMITerminal {
		
		private Element node = null;
		private String name;
		private String type;
		
		TransitionFeature(String type, String name, Element node) {
			super(type, name);
			this.node = node;
			this.type = type;
			this.name = name;
		}

		@Override
		public Element toXMI(Document doc) {
			Element guardNode = (Element) doc.adoptNode(node);
			return guardNode;
		}
		@Override
		public FSTNode getShallowClone() {
			return new TransitionFeature(type, name, node);
		}

		@Override
		public FSTNode getDeepClone() {
			return new TransitionFeature(type, name, node);
		}
	}

}
