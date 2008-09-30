package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import de.ovgu.cide.fstgen.ast.FSTNode;

public class XMITransition extends XMINonTerminal {
	
	public XMITransition(Element node, Element root) {
		super("XMI:Transition", node.getAttribute("name"), node, root);
		setNodeAttribute("isSpecification", node.getAttribute("isSpecification"));
		setNodeAttribute("xmi.id", node.getNodeName() + node.getAttribute("name"));
		setNodeAttribute("name", node.getAttribute("name"));
		
		//Transition source
		NodeList transSources = node.getElementsByTagName("UML:Transition.source");
		//Element transSource2 = (Element) transSources.item(0);
		Node transSource = transSources.item(0);
		Element transType = (Element) transSource.getChildNodes().item(1);
		addChild(new Target(transType, "UML:Transition.source"));
		
		//Transition target
		NodeList transTargets = node.getElementsByTagName("UML:Transition.target");
		Node transTarget = transTargets.item(0);
		transType = (Element) transTarget.getChildNodes().item(1);
		addChild(new Target(transType, "UML:Transition.target"));
		
		/*
		String newID = IdToElement(transType.getAttribute("xmi.idref"), transType.getNodeName());
		if (newID.equals("")) {
			newID = transType.getAttribute("xmi.idref");
		}
		setNodeAttribute("source", transType.getNodeName() + newID);
		
		
		
		NodeList simpleStates = transSource2.getElementsByTagName("UML:SimpleState");
		NodeList pseudoStates = transSource2.getElementsByTagName("UML:Pseudostate");
		NodeList finalStates = transSource2.getElementsByTagName("UML:FinalState");
		NodeList synchStates = transSource2.getElementsByTagName("UML:SynchState");
		NodeList compositeStates = transSource2.getElementsByTagName("UML:CompositeState");
		
		if (simpleStates.getLength() > 0) {
			Element simpleState = (Element) simpleStates.item(0);
			String newIDS = IdToElement(simpleState.getAttribute("xmi.idref"), "UML:SimpleState");
			if (newIDS.equals("")) {
				newIDS = simpleState.getAttribute("xmi.idref");
			}
			setNodeAttribute("source", "UML:SimpleState" + newIDS);
		} else if (pseudoStates.getLength() > 0) {
			Element pseudoState = (Element) pseudoStates.item(0);
			String newIDS = IdToElement(pseudoState.getAttribute("xmi.idref"), "UML:Pseudostate");
			setNodeAttribute("source", "UML:Pseudostate" + newIDS);
		} else if (synchStates.getLength() > 0) {
			Element synchState = (Element) synchStates.item(0);
			String newIDS = IdToElement(synchState.getAttribute("xmi.idref"), "UML:SynchState");
			setNodeAttribute("source", "UML:SynchState" + newIDS);
		} else if (compositeStates.getLength() > 0) {
			Element compState = (Element) compositeStates.item(0);
			String newIDS = IdToElement(compState.getAttribute("xmi.idref"), "UML:CompositeState");
			setNodeAttribute("source", "UML:CompositeState" + newIDS);
		}
		
		//Transition target
		NodeList transTargets = node.getElementsByTagName("UML:Transition.target");
		Element transTarget = (Element) transTargets.item(0);
		simpleStates = transTarget.getElementsByTagName("UML:SimpleState");
		pseudoStates = transTarget.getElementsByTagName("UML:Pseudostate");
		finalStates = transTarget.getElementsByTagName("UML:FinalState");
		synchStates = transTarget.getElementsByTagName("UML:SynchState");
		compositeStates = transTarget.getElementsByTagName("UML:CompositeState");
	
		
		if (simpleStates.getLength() > 0) {
			Element simpleState = (Element) simpleStates.item(0);
			String newIDT = IdToElement(simpleState.getAttribute("xmi.idref"), "UML:SimpleState");
			setNodeAttribute("target", "UML:SimpleState" + newIDT);
		} else if (pseudoStates.getLength() > 0) {
			Element pseudoState = (Element) pseudoStates.item(0); 
			String newIDT = IdToElement(pseudoState.getAttribute("xmi.idref"), "UML:Pseudostate");
			setNodeAttribute("target", "UML:PseudoState" + newIDT);
		} else if (synchStates.getLength() > 0) {
			Element synchState = (Element) synchStates.item(0); 
			String newIDT = IdToElement(synchState.getAttribute("xmi.idref"), "UML:SynchState");
			setNodeAttribute("target", "UML:SynchState" + newIDT);
		} else if (finalStates.getLength() > 0) {
			Element finalState = (Element) finalStates.item(0); 
			String newIDT = IdToElement(finalState.getAttribute("xmi.idref"), "UML:FinalState");
			setNodeAttribute("target", "UML:FinalState" + newIDT);
		} else if (compositeStates.getLength() > 0) {
			Element compState = (Element) compositeStates.item(0); 
			String newIDT = IdToElement(compState.getAttribute("xmi.idref"), "UML:CompositeState");
			setNodeAttribute("target", "UML:CompositeState" + newIDT);
		}*/
	
		//Transition trigger
		NodeList transTriggers = node.getElementsByTagName("UML:Transition.trigger");
		if (transTriggers.getLength() > 0) {
			Element transTrigger = (Element) transTriggers.item(0);
			NodeList signalEvents = transTrigger.getElementsByTagName("UML:SignalEvent");
			Element signalEvent = (Element) signalEvents.item(0);
			//String newIDT = IdToElement(simpleState.getAttribute("xmi.idref"), "UML:SimpleState");
			setNodeAttribute("signalevent", signalEvent.getAttribute("xmi.idref"));
		}
		
	}
	
	

	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement("UML:Transition");
		node.setAttribute("xmi.id", getNodeAttribute("xmi.id"));
		node.setAttribute("name", getNodeAttribute("name"));
		node.setAttribute("isSpecification", getNodeAttribute("isSpecification"));
		
		
		//Element transSource = doc.createElement("UML:Transition.source");
		//Element transTarget = doc.createElement("UML:Transition.target");
		Element transTrigger = doc.createElement("UML:Transition.trigger");
		//Element simpleStateS = doc.createElement("UML:SimpleState");
		//Element simpleStateT = doc.createElement("UML:SimpleState");
		Element signalEvent = doc.createElement("UML:SignalEvent");
		/*
		if (getNodeAttribute("source") != null) {
			simpleStateS.setAttribute("xmi.idref", getNodeAttribute("source"));
			transSource.appendChild(simpleStateS);
			node.appendChild(transSource);
		}
		
		if (getNodeAttribute("target") != null) {
			transTarget.appendChild(simpleStateT);
			simpleStateT.setAttribute("xmi.idref", getNodeAttribute("target"));
			node.appendChild(transTarget);
		}*/
		
		for (FSTNode fstNode : getChildren()) {
			XMINode xmiNode = (XMINode) fstNode;
			node.appendChild(xmiNode.toXMI(doc));
		}
		
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
		//clone.toFST();
		return clone;
	}
	
	private class Target extends XMITerminal {
		
		String type;
		Target(Element node, String type) {
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
			return new Target(getNode(), type);
		}

		@Override
		public FSTNode getDeepClone() {
			return new Target(getNode(), type);
		}
	}

}
