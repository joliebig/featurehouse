package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import de.ovgu.cide.fstgen.ast.FSTNode;

/**
 * Represents a state chart state. Possible types are: SimpleState,
 * Pseudostate, FinalState, CompositeState, SynchState
 */
public class XMIStateNode extends XMINonTerminal {

	static Integer counter = 0;

	String type;
	String name;

	public XMIStateNode(String type, String name, Element node, Element root) {
		super(type, name, node, root);
		this.type = type;
		this.name = name;
		setNodeAttribute("xmi.id", node.getAttribute("xmi.id"));
		setNodeAttribute("isSpecification", node
				.getAttribute("isSpecification"));
		//only in pseudo state
		setNodeAttribute("kind", node.getAttribute("kind"));
		//only in synch state
		setNodeAttribute("bound", node.getAttribute("bound"));
		//only in composite state
		setNodeAttribute("isConcurrent", node.getAttribute("isConcurrent"));
	}

	public void toFST() {
		NodeList children = getNode().getChildNodes();

		for (int i = 0; i < children.getLength(); i++) {
			Node node = children.item(i);
			//only immediate children of this node are of interest
			if (node.getParentNode() == getNode()) {
				if (node.getNodeName().equals("UML:StateVertex.outgoing")) {
					VertexOut vertexOut = new VertexOut(getNode(), getRoot());
					Element outTrans = (Element) node;
					NodeList outNodes = outTrans
							.getElementsByTagName("UML:Transition");
					for (int j = 0; j < outNodes.getLength(); j++) {
						vertexOut.addChild(new Transition((Element) outNodes
								.item(j)));
					}
					addChild(vertexOut);
				}

				if (node.getNodeName().equals("UML:StateVertex.incoming")) {
					VertexIn vertexIn = new VertexIn(getNode(), getRoot());
					Element inTrans = (Element) node;
					NodeList inNodes = inTrans
							.getElementsByTagName("UML:Transition");
					for (int j = 0; j < inNodes.getLength(); j++) {
						vertexIn.addChild(new Transition((Element) inNodes
								.item(j)));
					}
					addChild(vertexIn);
				}

				if (node.getNodeName().equals("UML:CompositeState.subvertex")) {

					NodeList nodes = ((Element) node).getChildNodes();
					for (int j = 0; j < nodes.getLength(); j++) {
						Node subNode = nodes.item(j);
						//only immediate children are of interest
						if (subNode.getParentNode() == node) {
							String type = subNode.getNodeName();

							if (!type.equals("#text")) {
								XMIStateNode xmiState = new XMIStateNode(type,
										((Element) subNode)
												.getAttribute("name"),
										(Element) subNode, getRoot());
								xmiState.toFST();
								addChild(xmiState);
							}
						}
					}
				}
			}
		}
		// any associated actions?
		for (XMIStateEvent stateEvent : XMIStateEvent.values()) {
			NodeList events = getNode().getElementsByTagName(
					"UML:State." + stateEvent.toString());
			if (events.getLength() > 0) {
				Element stateEventElement = (Element) events.item(0);
				if (stateEventElement.getParentNode() == getNode()) {
					addChild(new XMIStateAction(stateEventElement));

				}
			}
		}
	}

	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement(getType());
		if (!getName().equals("")) {
			node.setAttribute("xmi.id", type + getName());
		} else {
			node.setAttribute("xmi.id", type + getNodeAttribute("xmi.id"));
		}
		node.setAttribute("isSpecification",
				getNodeAttribute("isSpecification"));
		if (!getNodeAttribute("kind").equals("")) {
			node.setAttribute("kind", getNodeAttribute("kind"));
		}
		if (!getNodeAttribute("bound").equals("")) {
			node.setAttribute("bound", getNodeAttribute("bound"));
		}
		if (!getName().equals("")) {
			node.setAttribute("name", getName());
		}
		if (!getNodeAttribute("isConcurrent").equals("")) {
			node.setAttribute("isConcurrent", getNodeAttribute("isConcurrent"));
		}
		if (type.equals("UML:CompositeState")) {
			Element subvertex = doc
					.createElement("UML:CompositeState.subvertex");
			for (FSTNode fstnode : getChildren()) {
				XMINode xminode = (XMINode) fstnode;
				if (xminode instanceof VertexIn || xminode instanceof VertexOut) {
					node.appendChild(xminode.toXMI(doc));
				} else {
					subvertex.appendChild(xminode.toXMI(doc));
				}
			}

			if (subvertex.hasChildNodes()) {
				node.appendChild(subvertex);
			}
		} else {
			for (FSTNode fstNode : getChildren()) {
				if (fstNode instanceof XMINode) {
					node.appendChild(((XMINode) fstNode).toXMI(doc));
				}
			}
		}
		return node;
	}

	@Override
	public FSTNode getShallowClone() {
		return new XMIStateNode(type, name, getNode(), getRoot());
	}

	@Override
	public FSTNode getDeepClone() {
		XMIStateNode xmiState = new XMIStateNode(type, name, getNode(),
				getRoot());
		xmiState.toFST();
		return xmiState;
	}

	private class Transition extends XMITerminal {

		private Element node;

		Transition(Element node) {
			super("XMITransition", node.getNodeName()
					+ IdToElement(node.getAttribute("xmi.idref"), node
							.getNodeName()));

			this.node = node;
		}

		@Override
		public Element toXMI(Document doc) {
			Element node = doc.createElement("UML:Transition");
			node.setAttribute("xmi.idref", getName());
			return node;
		}

		@Override
		public FSTNode getShallowClone() {
			return new Transition(node);
		}

		@Override
		public FSTNode getDeepClone() {
			return new Transition(node);
		}
	}

	private class VertexOut extends XMINonTerminal {

		VertexOut(Element node, Element root) {
			super("XMIVertexOut", "", node, root);
		}

		void addVertex(Transition transition) {
			addChild(transition);
		}

		@Override
		public Element toXMI(Document doc) {
			Element node = doc.createElement("UML:StateVertex.outgoing");
			for (FSTNode fstnode : getChildren()) {
				node.appendChild(((Transition) fstnode).toXMI(doc));
			}
			return node;
		}

		@Override
		public FSTNode getShallowClone() {
			return new VertexOut(getNode(), getRoot());
		}

		@Override
		public FSTNode getDeepClone() {
			VertexOut vOut = new VertexOut(getNode(), getRoot());
			for (FSTNode child : getChildren()) {
				vOut.addChild(child);
			}
			return vOut;
		}
	}

	private class VertexIn extends XMINonTerminal {

		VertexIn(Element node, Element root) {
			super("XMIVertexIn", "", node, root);
		}

		void addVertex(Transition transition) {
			addChild(transition);
		}

		@Override
		public Element toXMI(Document doc) {
			Element node = doc.createElement("UML:StateVertex.incoming");
			for (FSTNode fstnode : getChildren()) {
				node.appendChild(((Transition) fstnode).toXMI(doc));
			}
			return node;
		}

		@Override
		public FSTNode getShallowClone() {
			return new VertexIn(getNode(), getRoot());
		}

		@Override
		public FSTNode getDeepClone() {
			VertexIn vIn = new VertexIn(getNode(), getRoot());
			for (FSTNode child : getChildren()) {
				vIn.addChild(child);
			}
			return vIn;
		}
	}

}
