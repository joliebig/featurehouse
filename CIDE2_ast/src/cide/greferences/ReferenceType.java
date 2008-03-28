package cide.greferences;

import cide.gast.ASTNode;

/**
 * abstraction of a reference type. describes a type of references between
 * ASTNodes, eg. method calles between method invocation nodes and method
 * declarations
 * 
 * references are always directed, but can happen between multiple classes
 * 
 * 
 * @author cKaestner
 * 
 */
public class ReferenceType implements IReferenceType {
	private String name;
	private Class<ASTNode>[] sourceClasses;
	private Class<ASTNode>[] targetClasses;

	public ReferenceType(String name, Class<ASTNode>[] sourceClasses,
			Class<ASTNode>[] targetClasses) {
		this.name = name;
		this.sourceClasses = sourceClasses;
		this.targetClasses = targetClasses;
	}

	public String toString() {
		return "ReferenceType:" + name;
	}
	
	@Override
	public int hashCode() {
		return name.hashCode();
	}
}
