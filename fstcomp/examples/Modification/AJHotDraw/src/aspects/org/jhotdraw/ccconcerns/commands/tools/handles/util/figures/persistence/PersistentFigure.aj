package org.jhotdraw.ccconcerns.figures.persistence;

import org.jhotdraw.framework.Figure;
import org.jhotdraw.standard.AbstractFigure;
import org.jhotdraw.util.Storable;
import org.jhotdraw.util.StorableInput;
import org.jhotdraw.util.StorableOutput;

/**
 * General aspect to add the persistence concern to the Figure
 * classes.
 * 
 * @author Marius M.
 *
 */
public aspect PersistentFigure {

	declare parents: Figure extends Storable;

	/**
	 * Declares <code>write(..)</code> for persistent figures. 
	 * Stores the Figure to a StorableOutput.
	 */
	public void AbstractFigure.write(StorableOutput dw) {
		// default implementation for a Figure's write method 
	}
       
	/**
	 * Declares <code>read(..)</code> for persistent figures. 
	 * Reads the Figure from a StorableInput.
	 */
	public void AbstractFigure.read(StorableInput dr) /*@AJHD refactored throws IOException*/ { 
		// default implementation for a Figure's read method
	}


}

