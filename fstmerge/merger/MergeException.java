package merger;

import java.util.List;

import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public class MergeException extends Exception {

	private static final long serialVersionUID = 8389564808113911652L;

	List<FSTNonTerminal> nonTerminalList;
	
	MergeException(List<FSTNonTerminal> tl) {
		super(); nonTerminalList = tl;
	}
	
	public String toString() {
		return "MergeException: invalid number of arguments: " + nonTerminalList.size();		
	}
}
