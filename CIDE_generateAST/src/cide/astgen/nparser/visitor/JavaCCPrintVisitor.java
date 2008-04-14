package cide.astgen.nparser.visitor;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import cide.astgen.nparser.ast.NAbstractValue;
import cide.astgen.nparser.ast.NChoice;
import cide.astgen.nparser.ast.NGrammar;
import cide.astgen.nparser.ast.NNonTerminal;
import cide.astgen.nparser.ast.NProduction;
import cide.astgen.nparser.ast.NTextOnly;
import cide.astgen.nparser.ast.NValue;
import cide.astgen.nparser.ast.NAbstractValue.Type;

public class JavaCCPrintVisitor extends NVisitor {

	private PrintStream out;

	public JavaCCPrintVisitor(PrintStream out) {
		this.out = out;
	}

	@Override
	public boolean visit(NProduction p) {
		out.println(p.getName() + " " + p.getName() + "() : { ");
		printParameter(p);
		out.println("\tToken firstToken=token;");
		out.print("} { (");
		return super.visit(p);
	}

	private void printParameter(NProduction p) {
		p.accept(new NVisitor() {
			@Override
			public boolean visit(NNonTerminal t) {
				return visitA(t);
			}

			public boolean visit(NTextOnly t) {
				if (t.type != Type.ONE)
					return visitA(t);
				return true;
			}

			public boolean visitA(NAbstractValue t) {
				String name = t.genVariableName();
				if (knownVariables.contains(name))
					return true;
				knownVariables.add(name);

				String init = "";
				if (t.type == Type.ZEROORONE)
					init = " = null";
				if (t.type == Type.ONEORMORE || t.type == Type.ZEROORMORE) {
					init = " = new " + t.genVariableType() + "()";
				}
				if (t.type == Type.ONEORMORE || t.type == Type.ZEROORMORE) {
					out.println("\t" + t.genVariablePlainType() + " "
							+ t.genVariablePlainName() + ";");
				}
				out.println("\t" + t.genVariableType() + " " + name + init
						+ ";");

				if (t.isListElement() && !printedList) {
					printedList = true;
					out.println("\tArrayList<" + t.genVariablePlainType()
							+ "> list" + t.getParent().getChoiceIdx()
							+ "=new ArrayList<" + t.genVariablePlainType()
							+ ">();");
				}
				return true;
			}

			Set<String> knownVariables = new HashSet<String>();

			private boolean hasToken = false;
			private boolean printedList = false;

			@Override
			public boolean visit(NValue t) {
				if (!hasToken) {
					hasToken = true;
					out.println("\tToken t;");
				}
				return visitA(t);
			}

			@Override
			public boolean visit(NChoice c) {
				printedList = false;
				return super.visit(c);
			}
		});
	}

	public void postVisit(NProduction g) {
		out.println();
		out.println(" ) }");
		out.println();
		super.postVisit(g);
	}

	@Override
	public boolean visit(NChoice c) {
		out.print("\n\t");
		return super.visit(c);
	}

	@Override
	public void postVisit(NChoice g) {
		out.print("\n\t{return new " + g.genClassname() + "(");
		printConstructorParameter(g);
		out.print("firstToken.next,token);}");

		if (g.getParent().getChoices().indexOf(g) != g.getParent().getChoices()
				.size() - 1)
			out.print(" |");
		super.postVisit(g);
	}

	private void printConstructorParameter(NChoice g) {
		if (g.isList())
			out.print("list" + g.getChoiceIdx() + ", ");
		g.accept(new NVisitor() {
			public boolean visit(NNonTerminal t) {
				return visitA(t);
			}

			public boolean visit(NValue t) {
				return visitA(t);
			}

			public boolean visit(NTextOnly t) {
				if (t.type != Type.ONE)
					return visitA(t);
				return true;
			}

			public boolean visitA(NAbstractValue t) {
				if (t.isListElement())
					return false;
				out.print(t.genVariableName());
				out.print(", ");
				return true;
			}
		});
	}

	@Override
	public boolean visit(NNonTerminal t) {
		visitA(t);
		return super.visit(t);
	}

	public boolean visit(NTextOnly t) {
		visitA(t);
		return super.visit(t);
	}

	public boolean visit(NGrammar t) {
		out.println(t.getIntroduction());
		return true;
	}

	@Override
	public boolean visit(NValue t) {
		visitA(t);
		return super.visit(t);
	}

	public boolean visitA(NAbstractValue t) {
		for (String x : t.outerPreTokens)
			printToken(x, true);
		switch (t.type) {
		case ZEROORONE:
			out.print("[");
			printInner(t);
			out.print("]");
			break;
		case ZEROORMORE:
			out.print("(");
			printInner(t);
			out.print(")*");
			break;
		case ONEORMORE:
			out.print("(");
			printInner(t);
			out.print(")+");
			break;
		case ONE:
		case OPTIONALWITHDEFAULT:
			printInner(t);
			break;
		default:
			throw new UnsupportedOperationException();
		}
		out.print(" ");
		for (String x : t.outerPostTokens)
			printToken(x, true);
		return true;
	}

	private void printToken(String x, boolean spaceAfter) {
		if (x.charAt(0) == '"')
			if (x.substring(0, 3).equals("\"!<")) {
				// special rule that fixed text tokens can be referenced from
				// inside
				out.print((spaceAfter ? "" : " ")
						+ x.substring(2, x.indexOf('>')+1)
						+ (spaceAfter ? " " : ""));
			} else {
				out
						.print((spaceAfter ? "" : " ") + x
								+ (spaceAfter ? " " : ""));
			}
		if (x.charAt(0) == 'L')
			out.print("LOOKAHEAD(" + removeQuotes(x.substring(1)) + ") ");
		if (x.charAt(0) == 'J')
			out.print(" {" + removeQuotes(x.substring(1)) + "} ");
	}

	private String removeQuotes(String name) {
		if (name.length() >= 2)
			if (name.charAt(0) == '"')
				return name.substring(1, name.length() - 1).replace("\\", "");
		return name;
	}

	private void printInner(NAbstractValue t) {
		for (String x : t.innerPreTokens)
			printToken(x, true);

		if (t instanceof NNonTerminal) {
			out.print(t.genVariablePlainName() + "=" + t.getName() + "()");
		}
		if (t instanceof NValue) {
			out.print("t=" + t.getName());
			out.print("{" + t.genVariablePlainName()
					+ "=new ASTStringNode(t.toString(),new WToken(t));}");
		}
		if ((t instanceof NTextOnly) && (t.type != Type.ONE)) {
			out.print("{" + t.genVariablePlainName() + "=new ASTTextNode("
					+ getTextTokenParameter(t) + ",new WToken(token));}");
		}

		if (t.isListElement()) {
			out.print("{list" + t.getParent().getChoiceIdx() + ".add("
					+ t.genVariablePlainName() + ");}");
		} else if (t.type == Type.ONEORMORE || t.type == Type.ZEROORMORE) {
			out.print("{" + t.genVariableName() + ".add("
					+ t.genVariablePlainName() + ");}");
		}

		for (String x : t.innerPostTokens)
			printToken(x, false);
	}

	private String getTextTokenParameter(NAbstractValue t) {
		List<String> tokens = new ArrayList<String>();
		for (String s : t.innerPreTokens)
			if (s.charAt(0) == '"')
				tokens.add(s);
		for (String s : t.innerPostTokens)
			if (s.charAt(0) == '"')
				tokens.add(s);

		if (tokens.size() == 0)
			return "";
		if (tokens.size() == 1)
			return tokens.get(0);

		boolean first = true;
		String result = "new String[]{";
		for (String s : tokens) {
			if (first)
				first = false;
			else
				result += ",";
			result += s;
		}
		result += "}";
		return result;
	}

}
