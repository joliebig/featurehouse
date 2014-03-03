package composer.rules.rtcomp.java;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.StringTokenizer;

import composer.rules.AbstractCompositionRule;
import composer.rules.JavaMethodOverriding;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class JavaRuntimeFunctionRefinement extends AbstractCompositionRule {
	
	private final String SWITCH_METHOD_ANNOTATION = JavaMethodOverriding.featureAnnotationPrefix + "featureSwitch\")\n";
	private final String SWITCH_ID_ANNOTATION = "@featureHouse.FeatureSwitchID(id=";
	private static int generatedSwitchMethods = 0;
	
	private static boolean addFeatureAnnotations = false;
	public static void setFeatureAnnotation(boolean addFeatureAnnotation) {
		JavaRuntimeFunctionRefinement.addFeatureAnnotations=addFeatureAnnotation;
	}

	@Override
	public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) {
			
			boolean aIsAbstract = !terminalA.getBody().contains("{");
			boolean bIsAbstract = !terminalB.getBody().contains("{");
			if(aIsAbstract && bIsAbstract) {
				// this is necessary, for example, if you want to combine conditional methods in interfaces (we must not generate bodies for switching code)
				terminalComp = terminalA;
				return;
			} else if (aIsAbstract != bIsAbstract) {
				throw new RuntimeException("Trying to combine an abstract method with a concrete one! (" + terminalA.getName() + ")");
			}
		
			FSTTerminal terminalComp2 = (FSTTerminal) terminalB.getDeepClone();
			FSTTerminal terminalComp3 = (FSTTerminal) terminalB.getDeepClone();
			nonterminalParent.addChild(terminalComp2);
			nonterminalParent.addChild(terminalComp3);
			
			int annotationEndB = JavaMethodOverriding.extractMethodAnnotationsEnd(terminalB.getBody());
			terminalB.setBody(terminalB.getBody().substring(annotationEndB));
			
			Signature sigB = Signature.fromString(terminalB.getBody());

			String toReplace = "original\\s*\\(";
			
			String featureNameTermA = terminalA.getOriginalFeatureName();
			
			String beforeFunctionName = sigB.name + "__before__" + featureNameTermA;
			String roleFunctionName = sigB.name + "__role__" + featureNameTermA;
			
			String newBody = terminalComp.getBody().replaceAll(toReplace, beforeFunctionName + "(");
			newBody = replaceFunctionName(roleFunctionName, sigB.name, newBody);
			
			if (addFeatureAnnotations) {
				//split the body of terminalComp2 in its major components; modify them seperately
				int methodNamePosition = JavaMethodOverriding.extractMethodPrefixEnd(newBody, sigB.name);
				int annotationsEnd = JavaMethodOverriding.extractMethodAnnotationsEnd(newBody);
				String prefix = newBody.substring(annotationsEnd, methodNamePosition);
				String restOfBody = newBody.substring(methodNamePosition);
				// replace old, copied annotations
				newBody = JavaMethodOverriding.featureAnnotationPrefix + featureNameTermA + "\")\n" +
						prefix + restOfBody;
			}
			terminalComp3.setBody(newBody);
			terminalComp3.setName(roleFunctionName);
			
			String newBody2 = replaceFunctionName(beforeFunctionName, sigB.name, terminalComp2.getBody());
			terminalComp2.setBody(newBody2);
			terminalComp2.setName(beforeFunctionName);
			
			String switchIdentifier = "verificationClasses.FeatureSwitches.__SELECTED_FEATURE_" + featureNameTermA;
			String newBody3 = "";
			String exceptions = "";
			
			if (! sigB.exceptions.isEmpty()) {
				exceptions = " throws ";
				for (int i = 0; i < sigB.exceptions.size(); i++) {
					if (i == sigB.exceptions.size()-1)
						exceptions += sigB.exceptions.get(i);
					else
						exceptions += sigB.exceptions.get(i)+ ", ";
				}
				
			}
			
			if (sigB.returnType.trim().endsWith("void")) {
				newBody3 = sigB.toString() + exceptions + " {\n" +
					"    if (" + switchIdentifier + ") {\n" +
					"        " + roleFunctionName + sigB.arglist + ";\n" + 
					"    } else {\n" +
					"        " + beforeFunctionName + sigB.arglist + ";\n" +
					"    }\n" +
					"}\n\n";
			} else {
				newBody3 = sigB.toString() + exceptions + " {\n" +
					"    if (" + switchIdentifier + ") {\n" +
					"        return " + roleFunctionName + sigB.arglist + ";\n" + 
					"    } else {\n" +
					"        return " + beforeFunctionName + sigB.arglist + ";\n" +
					"    }\n" +
					"}\n\n";
			}
			if (addFeatureAnnotations) {
				// featureNameTermB is not equal to JavaMethodOverriding.getFeatureName(terminalB), because this is the newest included feature; the method might come from an older one
				String featureNameTermB=extractMethodOriginFeature(newBody2);
				newBody3 = getSwitchMethodAnnotation(featureNameTermA, featureNameTermB) + newBody3;
			}
			terminalComp.setBody(newBody3);
	}

	private String extractMethodOriginFeature(String signature) {
		int annotationEnd = JavaMethodOverriding.extractMethodAnnotationsEnd(signature);
		signature = signature.replaceAll("\\s+\\(", "("); // removing all whitespace chars before opening parenthese
		int featureAnnotationBegin=signature.substring(0,annotationEnd).indexOf(JavaMethodOverriding.featureAnnotationPrefix);
		int featureNameBegin = featureAnnotationBegin+JavaMethodOverriding.featureAnnotationPrefix.length();
		int featureNameEnd = signature.substring(featureNameBegin, annotationEnd).indexOf("\"") + featureNameBegin;
		assert featureNameBegin < featureNameEnd;
		return signature.substring(featureNameBegin, featureNameEnd);
	}

	private String getSwitchMethodAnnotation(String thenFeature, String elseFeature) {
		return SWITCH_METHOD_ANNOTATION +
				SWITCH_ID_ANNOTATION + 
				(generatedSwitchMethods++) + 
				", thenFeature=\"" + thenFeature + "\""+ 
				", elseFeature=\"" + elseFeature + "\""+
				")\n";
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

		modPrefix = modPrefix.trim().replace(" ", "\\s+");
		// the \s+ guarantees that prefixes with multiple whitespaces between modifiers and returntype declaration are also matched
		return prefix + body.replaceFirst(modPrefix, "").replaceFirst(oldname, newname);
	}

	
	public static class Signature {
		
		public String returnType;
		public String name;
		public String paramlist;
		public String arglist;
		public List<String> exceptions;
		
		public Signature(String returnType, String name, String paramlist, List<String> exceptions) {
			this.returnType = returnType;
			this.name = name;
			this.paramlist = paramlist;
			this.exceptions = exceptions;
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
			List<String> exceptions = new ArrayList<String>();
			
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
			
			// this will not work well if the methodName or some parameter contains the String "throws "
			if (src.contains("throws ")) {
				String exs = src.substring(src.indexOf("throws ")+"throws".length());
				exs = exs.substring(0, exs.indexOf("{"));
				String[] exArr = exs.split(",");
				for (String s : exArr) {
					exceptions.add(s.trim());
				}
			}
			if (exceptions.isEmpty()) exceptions = Collections.emptyList();
			return new Signature(returnType, name, paramlist, exceptions);
		}
	}
	
}
