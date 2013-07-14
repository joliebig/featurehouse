package composer;

import de.ovgu.cide.fstgen.ast.FSTTerminal;

@SuppressWarnings("serial")
public class CompositionException extends Exception {

	private FSTTerminal terminalA; 
	private FSTTerminal terminalB;
	private String message;
	
	public CompositionException(FSTTerminal terminalA, FSTTerminal terminalB,
			String message) {
		this.terminalA = terminalA;
		this.terminalB = terminalB;
		this.message = message;
	}
	
	public FSTTerminal getTerminalA() {
		return terminalA;
	}
	
	public FSTTerminal getTerminalB() {
		return terminalB;
	}
	
	@Override
	public String getMessage() {
		return message;
	}

}
