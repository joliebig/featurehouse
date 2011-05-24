package cide.astgen.nparser.visitor;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import cide.astgen.nparser.ast.NChoice;
import cide.astgen.nparser.ast.NGrammar;

public class CreateReferenceManagerVisitor extends NCreateFileVisitor {

	// final Set<String> referenceTypes = new HashSet<String>();
	// final Set<String> refTargetTypes = new HashSet<String>();
	//	
	final Map<String, Set<String>> referenceSources = new HashMap<String, Set<String>>();
	final Map<String, Set<String>> referenceTargets = new HashMap<String, Set<String>>();

	public CreateReferenceManagerVisitor(File targetDir, String targetPackage)
			throws FileNotFoundException {
		super(targetDir, "ReferenceManager.java", targetPackage);
	}

	public CreateReferenceManagerVisitor(PrintStream stream,
			String targetPackage) {
		super(stream, targetPackage);
	}

	@Override
	public boolean visit(NGrammar g) {
		printHead();
		return super.visit(g);
	}

	@Override
	public void postVisit(NGrammar g) {
		assert isConsistent();
		printContent();
		printTail();
		super.postVisit(g);
	}

	void printContent() {
		if (!isConsistent())
			return;

		String types = "";
		for (String type : referenceSources.keySet()) {
			println("public final static ReferenceType " + genName(type)
					+ " = new ReferenceType(\"" + type + "\", "
					+ genClassList(referenceSources.get(type)) + ", "
					+ genClassList(referenceTargets.get(type)) + ");", 1);
			if (!types.equals(""))
				types += ", ";
			types += genName(type);
		}
		println("", 1);
		println("public ReferenceType[] getReferenceTypes() {", 1);
		println("return new ReferenceType[] { " + types + " };", 2);
		println("}", 1);
	}

	private String genClassList(Set<String> classNames) {
		String result = "new Class[] { ";
		boolean first = true;
		for (String className : classNames) {
			if (first)
				first = false;
			else
				result += ", ";
			result += className + ".class";
		}
		return result + " }";
	}

	public static String genName(String type) {
		return type.toLowerCase();
	}

	private void printHead() {
		if (!targetPackage.equals(""))
			println("package " + targetPackage + ";\n", 0);
		println("import java.util.*;", 0);
		println("import cide.greferences.*;\n", 0);
		println("import cide.gast.*;\n", 0);
		println("public class ReferenceManager implements IReferenceManager {",
				0);
	}

	@Override
	public boolean visit(NChoice c) {
		String className = c.genClassname();

		for (String ref : c.collectAnnotationValues("Reference")) {
			add(referenceSources, ref, className);
		}
		for (String ref : c.collectAnnotationValues("RefTarget")) {
			add(referenceTargets, ref, className);
		}
		return true;
	}

	private void add(Map<String, Set<String>> referenceMap, String ref,
			String className) {
		Set<String> classSet = referenceMap.get(ref);
		if (classSet == null) {
			classSet = new HashSet<String>();
			referenceMap.put(ref, classSet);
		}
		classSet.add(className);
	}

	private void printTail() {
		println("}", 0);
	}

	/**
	 * checks consistency, i.e. wether referenced types equal refTarget types
	 * and so on
	 */
	boolean isConsistent() {
		if (referenceSources.keySet().contains(""))
			return false;
		if (!referenceSources.keySet().equals(referenceTargets.keySet()))
			return false;
		return true;
	}

}
