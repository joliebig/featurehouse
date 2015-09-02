package composer.rules;

import composer.CompositionException;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

/**
 * 
 * @author Hendrik Speidel <speidel@fim.uni-passau.de>
 *
 */
public interface CompositionRule {

	/**
	 * 
	 * @return name of the composition rule e.g. "JavaMethodOverriding"
	 */
	public String getRuleName();
	
	
	/**
	 * composes terminal nodes terminalA and terminalB.
	 * The result must be stored in terminalComp (usually of the same type as terminalA and terminalB).
	 * If you want to keep both, add one to nonterminalParent and return the other as composition result.
	 * 
	 * If you don't do anything the result will be terminalA 
	 * (<code>terminalComp=terminalA.getShallowClone();</code> in <code>composer.FSTGenComposer.compose(...)</code>)
	 * 
	 * @param terminalA
	 * @param terminalB
	 * @param terminalComp
	 * @param nonterminalParent
	 * @throws Exception 
	 */
	public void compose(FSTTerminal terminalA, FSTTerminal terminalB,
			FSTTerminal terminalComp, FSTNonTerminal nonterminalParent) throws CompositionException;
	
	/**
	 * This method is called on the given terminal node before composition. 
	 * @param terminal
	 */
	public void preCompose(FSTTerminal terminal);

	/**
	 * This method is called on the given terminal node after composition. 
	 * @param terminal
	 */
	public void postCompose(FSTTerminal terminal);
	
}
