package builder.xmi;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import de.ovgu.cide.fstgen.ast.FSTNode;

public class XMICollaborationInteraction extends XMINonTerminal {
	
	private static Integer interactionCounter = 0;
	
	public XMICollaborationInteraction(Element node, Element root) {
		super("XMICollaborationInteraction" ,interactionCounter.toString(), node, root);
		NodeList interacions = node.getElementsByTagName("UML:Interaction");
		Element interaction = (Element) interacions.item(0);
		String name = interaction.getAttribute("name");
		if (name.equals("")) {
			System.err.println("Found a CollaborationInteraction without a name!");
		} else {
			setNodeAttribute("xmi.id", "CollborationInteraction" + name);
		}
		interactionCounter++;
	}
	
	public void toFST() {
		NodeList messages = getNode().getElementsByTagName("UML:Message");
		Element message = (Element) messages.item(0);
		InteractionMessage iMessage = new InteractionMessage(message, getRoot());
		iMessage.toFST();
		addChild(iMessage);	
	}

	@Override
	public Element toXMI(Document doc) {
		Element node = doc.createElement("UML:Collaboration.interaction");
		node.setAttribute("xmi.id", getNodeAttribute("xmi.id"));
	
		for (FSTNode fstNode : getChildren()) {
			node.appendChild(((XMINode) fstNode).toXMI(doc));
		}
		
		return node;
	}
	
	@Override
	public FSTNode getShallowClone() {
		return new XMICollaborationInteraction(getNode(), getRoot());
	}

	@Override
	public FSTNode getDeepClone() {
		XMICollaborationInteraction clone = new XMICollaborationInteraction(getNode(), getRoot());
		clone.toFST();
		return clone;
	}
	
	private class InteractionMessage extends XMINonTerminal {
		
		public InteractionMessage(Element node, Element root) {
			super("XMIInteractionMessage","", node, root);
			String name = node.getAttribute("name");
			if (name.equals("")) {
				System.err.println("Found an InteractionMessage without a name!");
			} else {
				setNodeAttribute("name", name);
				setNodeAttribute("xmi.id", "Message" + name);
			}
			
			setNodeAttribute("isSpecification", node.getAttribute("isSpecification"));
		}
		
		public void toFST() {
			NodeList senders = getNode().getElementsByTagName("UML:Message.sender");
			Element sender = (Element) senders.item(0);
			NodeList senderRoles = sender.getElementsByTagName("UML:ClassifierRole");
			Element senderRole = (Element) senderRoles.item(0);
			String senderRef = senderRole.getAttribute("xmi.idref");
			setNodeAttribute("sender", "ClassifierRole" + IdToElement(senderRef, "UML:ClassifierRole"));
			
			NodeList receivers = getNode().getElementsByTagName("UML:Message.receiver");
			Element receiver = (Element) receivers.item(0);
			NodeList receiverRoles = receiver.getElementsByTagName("UML:ClassifierRole");
			Element receiverRole = (Element) receiverRoles.item(0);
			String receiverRef = receiverRole.getAttribute("xmi.idref");
			setNodeAttribute("receiver", "ClassifierRole" + IdToElement(receiverRef, "UML:ClassifierRole"));
			
			NodeList connections = getNode().getElementsByTagName("UML:Message.communicationConnection");
			Element connection = (Element) connections.item(0);
			NodeList assocRoles = connection.getElementsByTagName("UML:AssociationRole");
			Element assocRole = (Element) assocRoles.item(0);
			String assocRef = assocRole.getAttribute("xmi.idref");
			//TODO: noch nicht optimal gelöst
			//setNodeAttribute("association", "AssociationRole" + IdToElement(assocRef, "UML:AssociationRole"));
			setNodeAttribute("association", assocRef);
		}

		@Override
		public Element toXMI(Document doc) {
			Element node = doc.createElement("UML:Interaction");
			Element interaction = doc.createElement("UML:Interaction.message");
			Element message = doc.createElement("UML:Message");
			message.setAttribute("name", getNodeAttribute("name"));
			message.setAttribute("isSpecification", getNodeAttribute("isSpecification"));
			
			message.setAttribute("xmi.id", getNodeAttribute("xmi.id"));
			message.setAttribute("name", getNodeAttribute("name"));
			message.setAttribute("isSpecification", getNodeAttribute("isSpecification"));
			
			// Message sender
			Element msgSender = doc.createElement("UML:Message.sender");
			Element senderClf = doc.createElement("UML:ClassifierRole");
			senderClf.setAttribute("xmi.idref", getNodeAttribute("sender"));
			msgSender.appendChild(senderClf);
			
			// Message receiver
			Element msgReceiver = doc.createElement("UML:Message.receiver");
			Element receiverClf = doc.createElement("UML:ClassifierRole");
			receiverClf.setAttribute("xmi.idref", getNodeAttribute("receiver"));
			msgReceiver.appendChild(receiverClf);
			
			// Message communication
			Element msgComm = doc.createElement("UML:Message.communicationConnection");
			Element assocRole = doc.createElement("UML:AssociationRole");
			assocRole.setAttribute("xmi.idref", getNodeAttribute("association"));
			msgComm.appendChild(assocRole);
			
			message.appendChild(msgSender);
			message.appendChild(msgReceiver);
			message.appendChild(msgComm);
			
			
			interaction.appendChild(message);
			node.appendChild(interaction);
			return node;
		}
		
		@Override
		public FSTNode getShallowClone() {
			return new InteractionMessage(getNode(), getRoot());
		}

		@Override
		public FSTNode getDeepClone() {
			InteractionMessage clone = new InteractionMessage(getNode(), getRoot());
			clone.toFST();
			return clone;
		}
	}
	

}
