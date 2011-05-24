package org.jhotdraw.ccconcerns.commands.undo;

import org.jhotdraw.ccconcerns.GenericRole;

import org.jhotdraw.standard.AbstractCommand;
import org.jhotdraw.util.Command;
import org.jhotdraw.util.Undoable;

/**
 * Undo support for Command elements
 * 
 * @author MariusMarin
 */
public aspect CommandUndo {

	//Command - UndoableRole role
	
	/**
	 * define the subject role for figure selection
	 */    
    public interface CommandUndoRole extends GenericRole {
    	public Undoable getUndoActivity();
    	public void setUndoActivity(Undoable newUndoableActivity);
    }

	declare parents: Command extends CommandUndoRole;

    private Undoable AbstractCommand.myUndoableActivity;   
    
	public Undoable AbstractCommand.getUndoActivity() {
		return myUndoableActivity;
	}

	public void AbstractCommand.setUndoActivity(Undoable newUndoableActivity) {
		myUndoableActivity = newUndoableActivity;
	}

	
}
