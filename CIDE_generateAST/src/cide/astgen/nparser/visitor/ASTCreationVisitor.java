package cide.astgen.nparser.visitor;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import cide.astgen.nparser.ast.NAbstractValue;
import cide.astgen.nparser.ast.NChoice;
import cide.astgen.nparser.ast.NNonTerminal;
import cide.astgen.nparser.ast.NProduction;
import cide.astgen.nparser.ast.NTextOnly;
import cide.astgen.nparser.ast.NAbstractValue.Type;

public class ASTCreationVisitor extends NVisitor {

	private File targetDirectory;

	private String targetPackage;

	private PrintStream out;

	public ASTCreationVisitor(File targetDirectory, String targetPackage) {
		this.targetDirectory = targetDirectory;
		this.targetPackage = targetPackage;
		createGenASTNode();
	}

	private void createGenASTNode() {
		out = getOutputFile("GenASTNode");
		printPackage(out);
		out.println("public abstract class GenASTNode extends ASTNode {");
		out
				.println("  public GenASTNode(Property[] p, Token firstToken, Token lastToken) {\n"
						+ "    super(p, new WToken(firstToken), new WToken(lastToken));");
		out.println("  }");
		out
				.println("  public GenASTNode(Property[] p, IToken firstToken, IToken lastToken) {\n"
						+ "    super(p, firstToken, lastToken);");
		out.println("  }");
		out
				.println("  public String toString() {\n"
						+ "    return this.getClass().getSimpleName() + \" \" + this.getStartPosition()\n"
						+ "        + \"-\" + (this.getStartPosition() + this.getLength());\n"
						+ "  }");
		out.println("  public String render() {\n"
				+ "    SimplePrintVisitor v=new SimplePrintVisitor();\n"
				+ "    accept(v);\n" + "    return v.getResult();\n" + "  }\n"
				+ "}");
	}

	@Override
	public boolean visit(NProduction p) {
		if (p.getChoices().size() > 1)
			createAbstractClass(p);
		return super.visit(p);
	}

	public boolean visit(NChoice c) {
		out = getOutputFile(c.genClassname());
		printPackage(out);
		out.print("public ");
		if (c.genSuperclass() == null)
			out.print("class " + c.genClassname() + " extends GenASTNode ");
		else {
			out.print("class " + c.genClassname() + " extends "
					+ c.genSuperclass() + " ");
		}
		if (c.getParent().isFirstProduction())
			out.print("implements ISourceFile ");
		out.println("{");
		List<NAbstractValue> units = applyASTGenerationAnnotation(getRelevantUnits(c.units));
		out.println("  public " + c.genClassname() + "(" + genParamList(units)
				+ "Token firstToken, Token lastToken) {\n"
				+ "    super(new Property[] {");
		for (int idx = 0; idx < units.size(); idx++) {
			out.println("      " + genPropertyConstr(units.get(idx))
					+ (idx < units.size() - 1 ? "," : ""));
		}
		out.println("    }, firstToken, lastToken);");
		out.println("  }");

		// clone method
		out
				.println("  public "
						+ c.genClassname()
						+ "(Property[] properties, IToken firstToken, IToken lastToken) {");
		out.println("    super(properties,firstToken,lastToken);");
		out.println("  }");
		out.println("  public ASTNode deepCopy() {");
		out.println("    return new " + c.genClassname()
				+ "(cloneProperties(),firstToken,lastToken);");
		out.println("  }");
		generateAccessMethods(units);
		out.print(generateReferenceMethod(c));
		return super.visit(c);
	}

	String generateReferenceMethod(NChoice c) {
		List<String> references = c.collectAnnotationValues("Reference");
		if (references.size() == 0)
			return "";
		String returnValue = "";
		boolean first = true;
		for (String refType : references) {
			if (first)
				first = false;
			else
				returnValue += ", ";
			returnValue += "ReferenceManager."
					+ CreateReferenceManagerVisitor.genName(refType);
		}
		return "  public IReferenceType[] getReferenceTypes() {\n    return new IReferenceType[]{ "
				+ returnValue + " };\n  }\n";
	}

	private void generateAccessMethods(List<NAbstractValue> units) {
		for (NAbstractValue unit : units) {
			out.println("  public " + unit.genVariableType() + " "
					+ unit.genAccessMethod() + "() {");
			out.println("    return ((" + getPropertyClass(unit)
					+ ")getProperty(\"" + unit.genPropertyName()
					+ "\")).getValue();");
			out.println("  }");
		}
	}

	private List<NAbstractValue> applyASTGenerationAnnotation(
			List<NAbstractValue> relevantUnits) {
		List<NAbstractValue> result = new ArrayList<NAbstractValue>(
				relevantUnits);

		/**
		 * transformation for lists: if &LI is used on one or more items, those
		 * are combined to a single item (ZeroOrMore) and placed at the front of
		 * the list.
		 */
		NAbstractValue listElement = null;
		for (Iterator iterator = result.iterator(); iterator.hasNext();) {
			NAbstractValue abstractValue = (NAbstractValue) iterator.next();
			if (abstractValue.isListElement()) {
				assert listElement == null
						|| listElement.getName()
								.equals(abstractValue.getName()) : "List elements must all be of the same type "
						+ abstractValue.getName()
						+ " vs "
						+ listElement.getName();
				listElement = abstractValue;
				iterator.remove();
			}
		}

		if (listElement != null) {
			listElement = listElement.cloneValue();
			listElement.type = Type.LIST;
			result.add(0, listElement);
		}

		return result;
	}

	private List<NAbstractValue> getRelevantUnits(List<NAbstractValue> units) {
		List<NAbstractValue> result = new ArrayList<NAbstractValue>();
		for (NAbstractValue unit : units)
			if (unit instanceof NTextOnly && unit.type == Type.ONE)
				;
			else
				result.add(unit);
		return result;
	}

	private void printPackage(PrintStream out) {
		if (targetPackage != null && !targetPackage.equals(""))
			out.println("package " + targetPackage + ";\n");

		out.println("import cide.gast.*;");
		out.println("import cide.gparser.*;");
		out.println("import cide.greferences.*;");
		out.println("import java.util.*;\n");
	}

	private PrintStream getOutputFile(String classname) {
		try {
			return new PrintStream(new File(targetDirectory, classname
					+ ".java"));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		return null;
	}

	private String genPropertyConstr(NAbstractValue value) {

		String propertyClass = getPropertyClass(value);

		String defaultParameter = "";
		if (value.type == Type.OPTIONALWITHDEFAULT)
			defaultParameter = ", " + value.defaultValue;

		String wrappeeParameter = "";
		if (value.isWrapper())
			wrappeeParameter = ", \""
					+ ((NNonTerminal) value).getWrappeePropertyName() + "\"";

		return "new " + propertyClass + "(\"" + value.genPropertyName()
				+ "\", " + value.genVariablePlainName() + defaultParameter
				+ wrappeeParameter + ")";
	}

	private String getPropertyClass(NAbstractValue value) {
		switch (value.type) {
		case ONEORMORE:
			return "PropertyOneOrMore<" + value.genVariablePlainType() + ">";
		case ZEROORMORE:
			return "PropertyZeroOrMore<" + value.genVariablePlainType() + ">";
		case ZEROORONE:
			return "PropertyZeroOrOne<" + value.genVariablePlainType() + ">";
		case OPTIONALWITHDEFAULT:
			return "PropertyOptionalWithDefault<"
					+ value.genVariablePlainType() + ">";
		case LIST:
			return "PropertyList<" + value.genVariablePlainType() + ">";
		case ONE:
			// if ONE we also care about wrappers
			if (value.isWrapper())
				return "PropertyWrapper<" + value.genVariablePlainType() + ","
						+ value.getWrapsAroundType() + ">";
			else
				return "PropertyOne<" + value.genVariablePlainType() + ">";
		}
		throw new UnsupportedOperationException();
	}

	private String genParamList(List<NAbstractValue> units) {
		String result = "";
		for (int idx = 0; idx < units.size(); idx++) {
			result += units.get(idx).genVariableType() + " "
					+ units.get(idx).genVariablePlainName();
			result += ", ";
		}
		return result;
	}

	@Override
	public void postVisit(NChoice g) {
		out.println("}");
		out.close();
		super.postVisit(g);
	}

	private String createAbstractClass(NProduction p) {
		PrintStream out = getOutputFile(p.getName());
		printPackage(out);
		out
				.println("public abstract class "
						+ p.getName()
						+ " extends GenASTNode "
						+ (p.isFirstProduction() ? "implements ISourceFile "
								: "")
						+ "{\n"
						+ "  protected "
						+ p.getName()
						+ "(Property[] p, Token firstToken, Token lastToken) { super(p, firstToken, lastToken); }\n"
						+ "  protected "
						+ p.getName()
						+ "(Property[] p, IToken firstToken, IToken lastToken) { super(p, firstToken, lastToken); }\n"
						+ "}");
		out.close();
		return p.getName();
	}

}
