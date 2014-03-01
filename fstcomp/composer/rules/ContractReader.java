package composer.rules;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import cide.gparser.CharStream;
import cide.gparser.OffsetCharStream;
import cide.gparser.ParseException;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;
import de.ovgu.cide.fstgen.parsers.generated_contract.ContractParser;

/**
 * A reader for contracts.
 * @author Fabian Benduhn
 *
 */
public class ContractReader {
	private ArrayList<FSTTerminal> ensuresClauses = new ArrayList<FSTTerminal>();
	private ArrayList<FSTTerminal> requiresClauses = new ArrayList<FSTTerminal>();
	private ArrayList<FSTTerminal> assignableClauses = new ArrayList<FSTTerminal>();
	
	public ContractReader(FSTTerminal contract){
		initClauses(contract);
	}
	
	public List<FSTTerminal> getEnsuresClauses(){
		return ensuresClauses;
	}
	public List<FSTTerminal> getRequiresClauses(){
		return requiresClauses;
	}
	public List<FSTTerminal> getAssignableClauses(){
		return assignableClauses;
	}
	/**
	 * Initializes the lists of clauses.
	 * @param terminalA
	 */
	private void initClauses(FSTTerminal terminal) {
		StringReader inputa = new StringReader(terminal.getBody());
		CharStream a = new OffsetCharStream(inputa);
		ContractParser pa = new ContractParser(a);
		try {
			pa.SpecCaseSeq(false);
		} catch (ParseException e) {
			e.printStackTrace();
		}

		ensuresClauses.clear();
		requiresClauses.clear();
		assignableClauses.clear();
		visitNode(pa.getRoot());
	
	}
	private void visitNode(FSTNode node) {
		if (node instanceof FSTNonTerminal) {
			for (FSTNode f : ((FSTNonTerminal) node).getChildren()) {
				visitNode(f);
			}
		}
		else if(node instanceof FSTTerminal){
			FSTTerminal t = (FSTTerminal) node;
			if(t.getType().equals("EnsuresClause")){
				ensuresClauses.add(t);
			}
			if(t.getType().equals("RequiresClause")){
				requiresClauses.add(t);
			}
			if(t.getType().equals("AssignableClause")){
				assignableClauses.add(t);
			}
		}
	}
}
