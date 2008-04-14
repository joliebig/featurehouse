package cide.astgen.nparser.ast;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import cide.astgen.nparser.visitor.NVisitor;

public abstract class NAbstractValue {
	public enum Type {
		ONE, ZEROORONE, ZEROORMORE, ONEORMORE, OPTIONALWITHDEFAULT, LIST
	}

	public static boolean isMultiType(Type t) {
		return t == Type.ZEROORMORE || t == Type.LIST || t == Type.ONEORMORE;
	}

	protected NAbstractValue(NChoice parent, String name, Type type) {
		this.parent = parent;
		this.type = type;
		this.name = name;
	}

	protected final NChoice parent;

	public Type type;

	public final List<String> outerPreTokens = new ArrayList<String>();

	public final List<String> innerPreTokens = new ArrayList<String>();

	public final List<String> outerPostTokens = new ArrayList<String>();

	public final List<String> innerPostTokens = new ArrayList<String>();

	public String defaultValue = null;// only used when
	// Type=OptionalWithDefault

	public boolean isWrappee = false;// only used when parent production is

	// used as a wrapper

	public abstract void accept(NVisitor visitor);

	public String genVariableType() {
		if (type == Type.ONEORMORE || type == Type.ZEROORMORE
				|| type == Type.LIST)
			return "ArrayList<" + genVariablePlainType() + ">";
		return genVariablePlainType();
	}

	public abstract String genVariablePlainType();

	public boolean isWrapper() {
		return false;
	}

	public String getWrapsAroundType() {
		return null;
	}

	public boolean isWrappee() {
		return isWrappee;
	}

	// Naming
	protected final String name;

	public String getName() {
		return name;
	}

	public String genVariableName() {
		if (type == Type.ONEORMORE || type == Type.ZEROORMORE
				|| type == Type.LIST)
			return genVariablePlainName() + "List";
		return genVariablePlainName();
	}

	public String genAccessMethod() {
		String name = genPropertyName();
		return "get" + Character.toUpperCase(name.charAt(0))
				+ name.substring(1);
	}

	public String genVariablePlainName() {
		int suffix = 0;
		for (int choiceIdx = 0; choiceIdx < parent.getParent().getChoices()
				.indexOf(parent); choiceIdx++) {
			for (NAbstractValue unit : parent.getParent().getChoices().get(
					choiceIdx).getUnits())
				if (unit.name.equals(name))
					suffix++;
		}
		for (int idx = 0; idx < parent.getUnits().indexOf(this); idx++) {
			NAbstractValue unit = parent.getUnits().get(idx);
			if (unit.getName().equals(getName()))
				suffix++;
		}

		String result = cleanJavaKeywords(name) + (suffix > 0 ? suffix : "");
		result = Character.toLowerCase(result.charAt(0)) + result.substring(1);

		return result;
	}

	public String genPropertyName() {
		return genVariablePlainName();
	}

	static final String[] javaKeywords = new String[] { "abstract", "continue",
			"for", "new", "switch", "assert", "default", "goto", "package",
			"synchronized", "boolean", "do", "if", "private", "this", "break",
			"double", "implements", "protected", "throw", "byte", "else",
			"import", "public", "throws", "case", "enum", "instanceof",
			"return", "transient", "catch", "extends", "int", "short", "try",
			"char", "final", "interface", "static", "void", "class", "finally",
			"long", "strictfp", "volatile", "const", "float", "native",
			"super", "while", "true", "false", "null" };

	/**
	 * checks whether items were named like java keywords. in these cases they
	 * have to be renamed attaching "_KW"
	 * 
	 * @param name
	 *            name to be tested
	 * @return cleaned name that is not a java keyword
	 */
	private String cleanJavaKeywords(String name) {
		String namel = name.toLowerCase();
		for (String keyword : javaKeywords)
			if (namel.equals(keyword))
				name += "_KW";
		return name;
	}

	public abstract NAbstractValue cloneValue();

	/**
	 * copies all values from the template object
	 */
	protected void adjustFrom(NAbstractValue template) {
		// this.name = template.name;
		// this.parent = template.parent;
		this.type = template.type;
		this.outerPreTokens.clear();
		this.outerPreTokens.addAll(template.outerPreTokens);
		this.innerPreTokens.clear();
		this.innerPreTokens.addAll(template.innerPreTokens);
		this.outerPostTokens.clear();
		this.outerPostTokens.addAll(template.outerPostTokens);
		this.innerPostTokens.clear();
		this.innerPostTokens.addAll(template.innerPostTokens);
		this.defaultValue = template.defaultValue;
		this.isWrappee = template.isWrappee;
	}

	public NChoice getParent() {
		return parent;
	}

	/**
	 * for the &LI list annotation
	 */
	public boolean isListElement() {
		return innerPreTokens.contains("&LI");
	}

	/**
	 * returns all annotations in the form
	 * 
	 * @X(Y)
	 */
	public List<String> getAnnotations() {

		List<String> result = new ArrayList<String>();
		result.addAll(innerPreTokens);
		result.addAll(innerPostTokens);
		result.addAll(outerPreTokens);
		result.addAll(outerPostTokens);
		for (Iterator<String> annotationIterator = result.iterator(); annotationIterator
				.hasNext();) {
			String annotation = (String) annotationIterator.next();
			if (annotation.charAt(0) != '@')
				annotationIterator.remove();
			else if (annotation.indexOf('(') < 0)
				annotationIterator.remove();
		}
		return result;
	}

	/**
	 * returns all annotations of a certain type
	 */
	public List<String> getAnnotations(String annoationType) {
		List<String> result = getAnnotations();
		for (Iterator<String> annotationIterator = result.iterator(); annotationIterator
				.hasNext();) {
			String fullAnnoation = (String) annotationIterator.next();
			String type = fullAnnoation
					.substring(1, fullAnnoation.indexOf('('));
			if (!type.equals(annoationType))
				annotationIterator.remove();
		}
		return result;
	}

	/**
	 * returns the parentezied part (value) of the annotations with a given type
	 */
	public List<String> getAnnotationValues(String annoationType) {
		List<String> result = new ArrayList<String>();
		List<String> annotations = getAnnotations(annoationType);
		for (String annotation : annotations) {
			result.add(annotation.substring(annotation.indexOf('(') + 1,
					annotation.indexOf(')')));
		}
		return result;
	}
	
	@Override
	public String toString() {
		return "["+name+"]";
	}
}
