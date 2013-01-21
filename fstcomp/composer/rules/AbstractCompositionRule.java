package composer.rules;

import de.ovgu.cide.fstgen.ast.FSTTerminal;

/**
 * 
 * @author Hendrik Speidel <speidel@fim.uni-passau.de>
 *
 *
 * Helper class that already implements getRuleName
 *
 */
abstract public class AbstractCompositionRule implements CompositionRule {

	/**
	 * Get the composition rule name e.g. "JavaMethodOverriding".
	 * 
	 * This implementation uses the simple name of the class
	 * e.g. in a subclass called JavaMethodOverriding
	 * this would return "JavaMethodOverriding".
	 * 
	 */
	public String getRuleName() {		
		return getClass().getSimpleName();
	}
	
	@Override
	public void preCompose(FSTTerminal terminalA) {
		// does nothing
	}

	@Override
	public void postCompose(FSTTerminal child) {
		// does nothing
	}
}
