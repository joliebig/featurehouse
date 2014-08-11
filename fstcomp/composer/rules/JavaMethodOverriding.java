package composer.rules;

import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.StringTokenizer;

import metadata.CompositionMetadataStore;

import composer.CompositionException;

import de.ovgu.cide.fstgen.ast.CommandLineParameterHelper;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class JavaMethodOverriding extends AbstractCompositionRule {

	private static boolean addFeatureAnnotations = false;

	public static final String featureAnnotationPrefix = "@featureHouse.FeatureAnnotation(name=\"";

	public final static String COMPOSITION_RULE_NAME = "JavaMethodOverriding";

	public static void setFeatureAnnotation(boolean addFeatureAnnotation) {
		JavaMethodOverriding.addFeatureAnnotations = addFeatureAnnotation;
	}

	private FSTNode getContractCompositionKeyword(FSTNode node) {
		if (node.getType().equals("ContractCompKey"))
			return node;

		if (node instanceof FSTNonTerminal) {
			for (FSTNode child : ((FSTNonTerminal) node).getChildren()) {
				FSTNode result = getContractCompositionKeyword(child);
				if (result != null)
					return result;
			}
		}

		return null;
	}

	private boolean isFinalContractMethod(FSTTerminal terminalB) {
		FSTTerminal contractCompKey = (FSTTerminal) getContractCompositionKeyword(terminalB
				.getParent());
		if (contractCompKey != null
				&& contractCompKey.getContractCompKey() != null
				&& contractCompKey.getContractCompKey()
						.equals("\\final_method"))
			return true;
		return false;
	}

	public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent)
			throws CompositionException {
		if (isFinalContractMethod(terminalB))
			throw new CompositionException(
					null,
					terminalA,
					"Previously you used the keyword \\final_method. Thus you can't refine this method or contract!");

		CompositionMetadataStore meta = CompositionMetadataStore.getInstance();

		specializeModifiers(terminalA, terminalB);

		if (!replaceOriginal(terminalA)) {
			terminalComp.setBody(terminalA.getBody());
			String funcName = meta.getMethodName(terminalA);
			meta.putMapping(funcName, (terminalA.getOriginalFeatureName()), funcName);
		} else {
			FSTTerminal terminalComp2 = null;
			FSTNonTerminal terminalParentComp2 = null;
			if (CommandLineParameterHelper.isJML()) {
				terminalParentComp2 = (FSTNonTerminal) (terminalB
						.getParent()).getDeepClone();
				((FSTNonTerminal) ((FSTNonTerminal) terminalComp.getParent())
						.getParent()).addChild(terminalParentComp2);
				terminalComp2 = (FSTTerminal) terminalParentComp2.getChildren()
						.get(2);
			} else {
				terminalComp2 = (FSTTerminal) terminalB.getDeepClone();
				nonterminalParent.addChild(terminalComp2);
			}
			String oldMethodName = terminalB.getName();

			StringTokenizer st = new StringTokenizer(oldMethodName, "(");
			if (st.hasMoreTokens()) {
				oldMethodName = st.nextToken();
			}
			st = new StringTokenizer(oldMethodName, " ");

			while (st.hasMoreTokens()) {
				oldMethodName = st.nextToken();
			}

			String toReplace = "original\\s*\\(";
			String newMethodName = oldMethodName + "__wrappee__"
					+ (terminalB.getOriginalFeatureName());
			String newBody = getNewBody(terminalA, terminalB, terminalComp,
					oldMethodName).replaceAll(toReplace, newMethodName + "(");
			if (addFeatureAnnotations) {
				newBody = JavaMethodOverriding.featureAnnotationPrefix
						+ (terminalA.getOriginalFeatureName()) + "\")\n" + newBody;
			}
			terminalComp.setBody(newBody);

			meta.putMapping(oldMethodName, (terminalB.getOriginalFeatureName()),
					newMethodName);
			meta.putMapping(oldMethodName, (terminalA.getOriginalFeatureName()),
					oldMethodName);

			// split the body of terminalComp2 in its major components; modify
			// them seperately
			int methodNamePosition = extractMethodPrefixEnd(
					terminalComp2.getBody(), oldMethodName);
			int annotationsEnd = extractMethodAnnotationsEnd(terminalComp2.getBody());

			String annotations = terminalComp2.getBody().substring(0, annotationsEnd);
			// remove override annotation from original method
			annotations = annotations.replaceAll("@Override", "");
			String prefix = terminalComp2.getBody().substring(annotationsEnd,methodNamePosition);
			String restOfBody = terminalComp2.getBody().substring(methodNamePosition);
			// prefix is the header from end of annotations to begin of method
			// name

			// Modify prefix
			prefix = prefix.replaceFirst("public", "private");
			prefix = prefix.replaceFirst("protected", "private");
			if (!prefix.contains("private") && !isC(nonterminalParent)) {
				prefix = "private " + prefix;
			}
			// modify the method name (is at beginning of restOfBody)
			restOfBody = restOfBody.replaceFirst(oldMethodName, newMethodName);
			// join the components
			terminalComp2.setBody(annotations + " " + prefix + " " + restOfBody);
			terminalComp2.setName(newMethodName);
			if (terminalParentComp2 != null)
				terminalParentComp2.setName(newMethodName);
		}
	}

	/**
	 * Extracts the position of the first letter after the annotations of this
	 * method declaration. (Defined as the first non-whitespace letter that is
	 * not an @ or immediately after an @, and not enclosed in parentheses.)
	 * 
	 * @featureHouse.FeatureAnnotation(name="verify") static void verify(Client
	 *                                                client, Email msg) Would
	 *                                                return the position of the
	 *                                                whitespace before
	 *                                                "static".
	 */
	public static int extractMethodAnnotationsEnd(String mBody) {
		int parenthesisLvl = 0;
		char[] auxArray = mBody.toCharArray();
		for (int i = 0; i < auxArray.length; i++) {
			if (auxArray[i] == '\\' && auxArray.length > i + 1
					&& (auxArray[i] == '(' || auxArray[i + 1] == ')')) {
				i++;
			} else if (auxArray[i] == '(') {
				parenthesisLvl++;
			} else if (auxArray[i] == ')') {
				parenthesisLvl--;
			} else if (parenthesisLvl == 0
					&& (i == 0 || auxArray[i - 1] == ' '
							|| auxArray[i - 1] == '\t'
							|| auxArray[i - 1] == '\n' || auxArray[i - 1] == ')')
					&& auxArray[i] != '@'
					&& !Character.isWhitespace(auxArray[i])) {
				return i;
			}
		}
		throw new InternalError(
				"Could not properly extract the position of the end of the annotations from method "
						+ mBody);
	}

	/**
	 * Extracts the position of the first letter of the methodName in this
	 * method Body. (Defined as the first occurence of the methodName that is
	 * not enclosed in parentheses. It could be enclosed in parentheses for
	 * example in annotations.)
	 * 
	 * @featureHouse.FeatureAnnotation(name="verify") static void verify(Client
	 *                                                client, Email msg) Would
	 *                                                return the position of the
	 *                                                second "verify" in the
	 *                                                string.
	 */
	public static int extractMethodPrefixEnd(String auxBody, String methodName) {
		int parenthesisLvl = 0;
		int mNameLen = methodName.length();
		char[] auxArray = auxBody.toCharArray();
		for (int i = 0; i <= auxArray.length - mNameLen; i++) {
			if (auxArray[i] == '\\' && auxArray.length > i + 1
					&& (auxArray[i] == '(' || auxArray[i + 1] == ')')) {
				i++;
			} else if (auxArray[i] == '(') {
				parenthesisLvl++;
			} else if (auxArray[i] == ')') {
				parenthesisLvl--;
			} else if (parenthesisLvl == 0
					&& (i == 0 || auxArray[i - 1] == ' '
							|| auxArray[i - 1] == '\t' || auxArray[i - 1] == '\n')
					&& methodName.equals(auxBody.substring(i, i + mNameLen))) {
				return i;
			}
		}
		throw new InternalError(
				"Could not properly extract the position of the methodName from method "
						+ methodName);
	}

	protected String getNewBody(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, String oldMethodName) {
		return terminalComp.getBody();
	}

	/**
	 * @param terminalA
	 * @return
	 */
	protected boolean replaceOriginal(FSTTerminal terminalA) {
		return terminalA.getBody().matches("(?s).*\\s*original\\s*.*");
	}

	@Override
	public String getRuleName() {
		return COMPOSITION_RULE_NAME;
	}

	private static boolean isC(FSTNode node) {
		if (node.getType().equals("C-File")) {
			return true;
		} else {
			FSTNode parent = node.getParent();
			if (parent != null) {
				return isC(parent);
			} else {
				return false;
			}
		}

	}

	private static void specializeModifiers(FSTTerminal terminalA,
			FSTTerminal terminalB) {

		if (terminalA.getBody().contains("@")
				|| terminalB.getBody().contains("@"))
			return;

		StringTokenizer stA = new StringTokenizer(terminalA.getBody(), "(");
		StringTokenizer stB = new StringTokenizer(terminalB.getBody(), "(");

		if (stA.hasMoreTokens() && stB.hasMoreTokens()) {
			stA = new StringTokenizer(stA.nextToken(), " ");
			LinkedHashSet<String> modifierSetA = new LinkedHashSet<String>();
			while (stA.hasMoreTokens()) {
				modifierSetA.add(stA.nextToken());
			}
			stB = new StringTokenizer(stB.nextToken(), " ");
			LinkedHashSet<String> modifierSetB = new LinkedHashSet<String>();
			while (stB.hasMoreTokens()) {
				modifierSetB.add(stB.nextToken());
			}
			String[] modifierArrayA = new String[modifierSetA.size()];
			modifierSetA.toArray(modifierArrayA);

			String[] modifierArrayB = new String[modifierSetB.size()];
			modifierSetB.toArray(modifierArrayB);

			if (modifierArrayA.length <= 1 || modifierArrayB.length <= 1)
				return;

			if (!modifierArrayA[modifierArrayA.length - 2]
					.equals(modifierArrayB[modifierArrayB.length - 2])) {
				System.err.println("Warning: return types of the two methods `"
						+ modifierArrayA[modifierArrayA.length - 1]
						+ "' are not compatible: "
						+ modifierArrayA[modifierArrayA.length - 2] + " != "
						+ modifierArrayB[modifierArrayB.length - 2]);
			}

			String removedDuplicates = new String();
			String[] modifierArrayRes = new String[modifierArrayA.length
					+ modifierArrayB.length - 2];
			System.arraycopy(modifierArrayB, 0, modifierArrayRes, 0,
					modifierArrayB.length - 2);
			System.arraycopy(modifierArrayA, 0, modifierArrayRes,
					modifierArrayB.length - 2, modifierArrayA.length);

			boolean isPublic = false;
			boolean isProtected = false;
			boolean isPrivate = false;
			boolean isAbstract = false;
			boolean isFinal = false;
			LinkedList<String> otherModifiers = new LinkedList<String>();
			for (int i = 0; i < modifierArrayRes.length; i++) {
				String modifier = modifierArrayRes[i].trim();
				if (modifier.equals("private") && !isPublic && !isProtected
						&& !isPrivate) {
					isPrivate = true;
					removedDuplicates += modifier + " ";
				} else if (modifier.equals("protected") && !isPublic
						&& !isProtected) {
					isProtected = true;
					removedDuplicates = removedDuplicates.replaceAll("private",
							"");
					removedDuplicates += modifier + " ";
				} else if (modifier.equals("public") && !isPublic) {
					isPublic = true;
					removedDuplicates = removedDuplicates.replaceAll("private",
							"");
					removedDuplicates = removedDuplicates.replaceAll(
							"protected", "");
					removedDuplicates += modifier + " ";
				} else if (!modifier.equals("public")
						&& !modifier.equals("protected")
						&& !modifier.equals("private")) {
					if (modifier.equals("abstract") && !isAbstract) {
						isAbstract = true;
						removedDuplicates = removedDuplicates.replaceAll(
								"final", "");
						removedDuplicates += modifier + " ";
					} else if (modifier.equals("final") && !isAbstract
							&& !isFinal) {
						removedDuplicates += modifier + " ";
						isFinal = true;
					} else if (!modifier.equals("abstract")
							&& !modifier.equals("final")) {

						boolean in = false;
						for (String otherModifier : otherModifiers) {
							if (otherModifier.equals(modifier))
								in = true;
						}
						if (!in) {
							removedDuplicates += modifier + " ";
							otherModifiers.add(modifier);
						}
					}
				}
			}
			if (terminalA.getBody().contains("("))
				terminalA.setBody(removedDuplicates
						+ " "
						+ terminalA.getBody().substring(
								terminalA.getBody().indexOf("(")));

			if (terminalB.getBody().contains("("))
				terminalB.setBody(removedDuplicates
						+ " "
						+ terminalB.getBody().substring(
								terminalB.getBody().indexOf("(")));

		}
	}
}
