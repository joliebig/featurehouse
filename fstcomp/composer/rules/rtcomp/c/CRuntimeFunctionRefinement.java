package composer.rules.rtcomp.c;

import java.util.StringTokenizer;

import composer.rules.AbstractCompositionRule;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class CRuntimeFunctionRefinement extends AbstractCompositionRule {

	@Override
	public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {

			FSTTerminal terminalComp2 = (FSTTerminal) terminalB.getDeepClone();
			FSTTerminal terminalComp3 = (FSTTerminal) terminalB.getDeepClone();
			nonterminalParent.addChild(terminalComp2);
			nonterminalParent.addChild(terminalComp3);
			
			Signature sigB = Signature.fromString(terminalB.getBody());

			String toReplace = "original\\s*\\(";
			
			String featureName = getFeatureName(terminalA);
			String beforeFunctionName = sigB.name + "__before__" + featureName;
			String roleFunctionName = sigB.name + "__role__" + featureName;
			
			
			String newBody = terminalComp.getBody().replaceAll(toReplace, beforeFunctionName + "(");
			newBody = replaceFunctionName(roleFunctionName, sigB.name, newBody);
			terminalComp3.setBody(newBody);
			terminalComp3.setName(roleFunctionName);
			
			String newBody2 = replaceFunctionName(beforeFunctionName, sigB.name, terminalComp2.getBody());			
			terminalComp2.setBody(newBody2);
			terminalComp2.setName(beforeFunctionName);
			
			String switchIdentifier = "__SELECTED_FEATURE_" + featureName;
			String newBody3 = "";
			if (sigB.returnType.trim().equals("void")) {
				newBody3 = sigB.toString() + " {\n" +
					"    if (" + switchIdentifier + ") {\n" +
					"        " + roleFunctionName + sigB.arglist + ";\n" + 
					"    } else {\n" +
					"        " + beforeFunctionName + sigB.arglist + ";\n" +
					"    }\n" +
					"}\n\n";
			} else {
				newBody3 = sigB.toString() + " {\n" +
					"    if (" + switchIdentifier + ") {\n" +
					"        return " + roleFunctionName + sigB.arglist + ";\n" + 
					"    } else {\n" +
					"        return " + beforeFunctionName + sigB.arglist + ";\n" +
					"    }\n" +
					"}\n\n";
			}
			terminalComp.setBody(newBody3);
				
	}
	
	public static String getFeatureName(FSTNode node) {
		if (node.getType().equals("Feature"))
			return node.getName();
		else
			return getFeatureName(node.getParent());
	}
	

	@Override
	public String getRuleName() {
		return "JavaMethodOverriding";
	}
	
	public String replaceFunctionName(String newname, String oldname, String body) {
		String auxBody = "";
		StringTokenizer st = new StringTokenizer(body, "(");
		if (st.hasMoreTokens()) {
			auxBody = st.nextToken();
		}
		
		st = new StringTokenizer(auxBody);
		String prefix = "";
		boolean found = false;
		while (st.hasMoreTokens() && !found) {
			String token = st.nextToken();
			if (oldname.equals(token)) {
				found = true;
			} else {
				prefix += token + " ";
			}
		}

		// Replace '[]' for regex
		String modPrefix = "";
		for (char c : prefix.toCharArray()) {
			if (c == '[')
				modPrefix += "\\[";
			else if (c == ']')
				modPrefix += "\\]";
			else if (c == '*')
				modPrefix += "\\*";
			else
				modPrefix += String.valueOf(c);
		}

		modPrefix = modPrefix.trim();
		return prefix + body.replaceFirst(modPrefix, "").replaceFirst(oldname, newname);
	}

	
	public static class Signature {
		
		public String returnType;
		public String name;
		public String paramlist;
		public String arglist;
		
		public Signature(String returnType, String name, String paramlist) {
			this.returnType = returnType;
			this.name = name;
			this.paramlist = paramlist;
			parseArgs();
		}
		
		public String toString() {
			return returnType + "\n" + name + paramlist;
		}
		
		private void parseArgs() {
			String params = paramlist.substring(1, paramlist.length() - 1);
			String[] parts = params.split(",");
			String arglist = "(";
			for (int i = 0; i < parts.length; i++) {
				String[] items = parts[i].trim().split("\\s");
				String arg = items[items.length - 1];
				if (arg.startsWith("*")) {
					arg = arg.substring(1);
				}
				arglist += arg;
				if (i + 1 < parts.length) {
					arglist += ", ";
				}
			}
			arglist += ")";
			this.arglist = arglist;
		}
		
		public static Signature fromString(String src) {
			String name = src;
			String returnType = "";
			String paramlist = "()";
			
			StringTokenizer st = new StringTokenizer(name, "(");
			if (st.hasMoreTokens()) {
				name = st.nextToken();
			} else {
				System.err.println("mysterious function sig :" + src);
			}
			if (st.hasMoreTokens()) {
				paramlist = "(" + st.nextToken().split("\\)")[0] + ")";
			} else {
				System.err.println("mysterious function sig :" + src);
			}
			
			
			String[] parts = name.trim().split("\\s"); 
			name = parts[parts.length -1];
			
			for (int i = 0, ii = parts.length - 1; i < ii; i++) {
				returnType += parts[i];
				if (i + 1 < ii) {
					returnType += " ";
				}
			}
			
			if (name.startsWith("*")) {
				name = name.substring(1);
				returnType += "*";
			}
			
			return new Signature(returnType, name, paramlist);
		}
	}
	
}
