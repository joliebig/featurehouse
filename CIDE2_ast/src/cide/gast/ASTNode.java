package cide.gast;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import cide.greferences.IReferenceType;

public abstract class ASTNode implements IASTNode {

	public class StartPositionSorter implements Comparator<ASTNode> {

		public int compare(ASTNode o1, ASTNode o2) {
			if (o1.getStartPosition() < o2.getStartPosition())
				return -1;
			if (o1.getStartPosition() > o2.getStartPosition())
				return 1;
			return 0;
		}

	}

	protected final List<Property> properties;

	public IToken firstToken;

	public IToken lastToken;

	protected ASTNode(List<Property> properties, IToken firstToken,
			IToken lastToken) {
		this.properties = properties;
		this.firstToken = firstToken;
		this.lastToken = lastToken;
		for (Property p : properties)
			p.setParent(this);
	}

	protected ASTNode(Property[] properties, IToken ft, IToken lt) {
		this(Arrays.asList(properties), ft, lt);
	}

	public void accept(IASTVisitor visitor) {
		if (visitor.visit(this)) {
			List<ASTNode> children = new ArrayList<ASTNode>();

			for (Property property : properties)
				for (ASTNode child : property.getChildren())
					children.add(child);

			Collections.sort(children, new StartPositionSorter());

			for (ASTNode child : children)
				child.accept(visitor);
		}
		visitor.postVisit(this);
	}

	public Property getProperty(String name) {
		for (Property property : properties)
			if (property.name.equals(name))
				return property;
		return null;
	}

	public ISourceFile getRoot() {
		ASTNode parent = this;
		while (parent.getParent() != null) {
			parent = parent.getParent();
		}
		return (ISourceFile) parent;
	}

	private ASTNode parentNode;

	private Property parentProperty;

	void setParent(ASTNode parentNode, Property parentProperty) {
		this.parentNode = parentNode;
		this.parentProperty = parentProperty;
	}

	public ASTNode getParent() {
		return parentNode;
	}

	public Property getLocationInParent() {
		return parentProperty;
	}

	private String idCache = null;

	public String getId() {
		if (idCache != null)
			return idCache;
		String id = "";
		if (parentNode != null)
			id = parentNode.getId() + "/" + parentProperty.getId(this);
		idCache = id;
		return id;
	}

	public List<Property> getProperties() {
		return Collections.unmodifiableList(properties);
	}

	public int getStartPosition() {
		return firstToken.getOffset();
	}

	public int getLength() {
		if (lastToken.getOffset() == firstToken.getOffset())
			return firstToken.getLength();
		return lastToken.getOffset() + lastToken.getLength()
				- getStartPosition();
	}

	public boolean isOptional() {
		if (parentProperty == null)
			return false;
		return parentProperty.canRemoveSubtree(this);
	}

	protected Property[] cloneProperties() {
		Property[] result = new Property[properties.size()];
		int i = 0;
		for (Property p : properties)
			result[i++] = p.deepCopy();
		return result;
	}

	public abstract ASTNode deepCopy();

	public void remove() {
		if (!isOptional())
			return;

		parentProperty.removeSubtree(this);
	}

	public boolean hasReferenceTypes() {
		return getReferenceTypes().length > 0;
	}

	/**
	 * returns all supported reference types for this choice.
	 * 
	 * @return array of reference types, possibly empty
	 */
	public IReferenceType[] getReferenceTypes() {
		return new IReferenceType[0];
	}

}