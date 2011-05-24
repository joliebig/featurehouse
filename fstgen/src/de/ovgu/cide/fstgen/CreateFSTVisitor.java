package de.ovgu.cide.fstgen;

import java.util.HashSet;
import java.util.Set;

import cide.astgen.nparser.ast.NAbstractValue;
import cide.astgen.nparser.ast.NAnnotation;
import cide.astgen.nparser.ast.NChoice;
import cide.astgen.nparser.ast.NGrammar;
import cide.astgen.nparser.ast.NNonTerminal;
import cide.astgen.nparser.ast.NProduction;
import cide.astgen.nparser.ast.NValue;
import cide.astgen.nparser.visitor.NVisitor;
import de.ovgu.cide.fstgen.ast.FSTNodeType;

public class CreateFSTVisitor extends NVisitor {
	private boolean isWellformed;
	private String errorMsg;
	private Set<FSTNodeType> fstNodeTypes = new HashSet<FSTNodeType>();
	private Set<NChoice> generatedTypes = new HashSet<NChoice>();

	public CreateFSTVisitor() {
		isWellformed = true;
		errorMsg = "";
	}

	public String printDebug() {
		return "";
	}

	// private HashMap<NProduction,NProduction> referencedNonTerminal = new
	// HashMap<NProduction,NProduction>();

	@Override
	public boolean visit(NChoice choice) {
		NAnnotation nonTerminalAnnotation = choice
				.findAnnotation("FSTNonTerminal");
		NAnnotation terminalAnnotation = choice.findAnnotation("FSTTerminal");
		NGrammar grammar = choice.getParent().getGrammar();

		checkWellformedness(choice, nonTerminalAnnotation, terminalAnnotation,
				grammar);

		genType(choice, nonTerminalAnnotation, terminalAnnotation, grammar);

		return super.visit(choice);
	}

	private void genType(NChoice choice, NAnnotation nonTerminalAnnotation,
			NAnnotation terminalAnnotation, NGrammar grammar) {
		if (nonTerminalAnnotation != null) {
			fstNodeTypes.add(new FSTNodeType(choice.genClassname(), false));
			generatedTypes.add(choice);

			// search for terminal children
			for (NAbstractValue unit : choice.getUnits()) {
				if (unit instanceof NNonTerminal) {
					NProduction referencedProduction = grammar
							.findProduction(((NNonTerminal) unit).getName());
					if (referencedProduction != null)
						for (NChoice targetChoice : referencedProduction
								.getChoices()) {

							if (targetChoice.findAnnotation("FSTNonTerminal") == null)
								makeNonTerminalType(targetChoice);
						}
				}
			}
		}

		// if root is already non-terminal?
		if (nonTerminalAnnotation == null
				&& choice.getParent().isFirstProduction()) {
			makeNonTerminalType(choice);
		}
	}

	private void makeNonTerminalType(NChoice choice) {
		if (!generatedTypes.contains(choice)) {
			generatedTypes.add(choice);
			fstNodeTypes.add(new FSTNodeType(choice.genClassname(), true));
		}
	}

	private void checkWellformedness(NChoice p,
			NAnnotation nonTerminalAnnotation, NAnnotation terminalAnnotation,
			NGrammar grammar) {
		// must not be terminal and nonterminal at the same time
		if (nonTerminalAnnotation != null && terminalAnnotation != null)
			markNotWellformed("Choice "
					+ p.genClassname()
					+ " in production "
					+ p.getParent().getName()
					+ " cannot be a terminal and a non-terminal at the same time.");

		// non-terminals may not include <XY> tokens (NValue), but those must be
		// extracted to terminal productions.
		if (nonTerminalAnnotation != null) {
			for (NAbstractValue unit : p.getUnits()) {
				if (unit instanceof NValue && !"<EOF>".equals(unit.getName()) && !"<NONE>".equals(unit.getName()))
					markNotWellformed("Non-terminal choice " + p.genClassname()
							+ " in production " + p.getParent().getName()
							+ " must not contain the token reference "
							+ unit.getName());
			}
		}

		// must not reference a non-terminal from a terminal
		if (nonTerminalAnnotation == null) {
			for (NAbstractValue unit : p.getUnits()) {
				if (unit instanceof NNonTerminal) {
					NProduction referencedProduction = grammar
							.findProduction(((NNonTerminal) unit).getName());
					if (referencedProduction != null) {
						for (NChoice c : referencedProduction.getChoices())
							if (c.findAnnotation("FSTNonTerminal") != null)
								markNotWellformed("Terminal choice "
										+ p.genClassname()
										+ " in production "
										+ p.getParent().getName()
										+ " must not reference non-terminal choice "
										+ c.genClassname() + " in "
										+ referencedProduction.getName());
					}
				}
			}
		}

//		if (nonTerminalAnnotation != null
//				&& nonTerminalAnnotation.values.get("name") == null)
//			markNotWellformed("Non-terminal choice " + p.genClassname()
//					+ " in production " + p.getParent().getName()
//					+ " must have a name.");
	}

	private void markNotWellformed(String string) {
		isWellformed = false;
		errorMsg += string + "\n";
	}

	/**
	 * to be wellformed the following rules must apply: * non-terminals can only
	 * be reached from other non-terminals or the top-level production *
	 * non-terminals must be named
	 * 
	 * @return
	 */
	public boolean hasWellformedFSTAnnotations() {
		return isWellformed;
	}

	public String getWellformedErrorMsg() {
		return errorMsg;
	}

	public Set<FSTNodeType> getFSTNodeTypes() {
		return fstNodeTypes;
	}
}
