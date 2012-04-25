package de.ovgu.cide.fstgen.fstProcessing;

import java.util.ArrayList;
import java.util.List;

import de.ovgu.cide.fstgen.ast.FSTNode;

/** modifies a FST after parsing. Reverse modification must be done after composition.
 * <p> Useful to keep the grammar simple / to be able to
 * reuse an existing grammar and generate slightly different FSTs out of it
 * <p>Processing FSTs is useful if one of the following reasons apply:<ul>
 * <li>There is a need for manually touching the FST.
 * <li>if a language does not conform to a container structure:
 * <br><i>JML annotations occur before the corresponding method declaration. 
 * To model pre- and postconditions as children of a JML-annotated method, 
 * FST postprocessing moves the JML annotations to be child nodes of the method. 
 * Every method is to be marked as FSTNonTerminal node with a FSTTerminal 
 * containing its code as well.</i>
 * </ul>
 * <p>The step by step procedure is as follows:<ol>
 * <li>parsing
 * <li>FST construction
 * <li><code>FSTprocessor.processFST</code>
 * <li>composition
 * <li><code>FSTprocessor.reconstructFST</code>
 * <li>pretty printing
 * </ol>
 * To issue FST processing, call <code>FSTGenComposer</code> with command line argument
 * <code>-fstprocessing</code> and the name of the concrete <code>FSTprocessor</code> class.
 * @author Wolfgang Scholz */
public interface FSTprocessor {
	
	/** Does some postprocessing on the given FST. 
	 * <p>Note that FST processing needs to be done <i>before</i> composition.
	 * @param fst - FST to process (will be changed by the call)
	 * @see FSTprocessor#reconstructFST(ArrayList)
	 */
	public void processFST(List<FSTNode> fst);
	
	
	/**
	 * Reverses the processing done by <code>processFST</code>.
	 * <p>This step is needed, as a pretty printer is obtained from the original grammar
	 * which does not consider the processing done by this <code>FSTprocessor</code>.
	 * Processing only should have impact on composition, thus composition should be
	 * the only step which is applied to the processed FST.
	 * <p>Note that FST reconstruction needs to be done <i>after</i> composition.
	 * @param fst - FST to reverse the processing step on (will be changed by the call)
	 * @see FSTprocessor#processFST(ArrayList)
	 */
	public void reconstructFST(List<FSTNode> fst);
}
