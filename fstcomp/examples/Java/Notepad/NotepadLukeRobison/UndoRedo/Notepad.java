

import javax.swing.undo.*;

class Notepad {

    UndoManager undo = new UndoManager();
	UndoAction undoAction = new UndoAction( this );
    RedoAction redoAction = new RedoAction( this );
	private JButton undoButton, redoButton;

	Notepad() {
        ediT.add( undoAction );
        ediT.add( redoAction );
        toolBar.add( undoAction );
        toolBar.add( redoAction );

        textArea.getDocument().addUndoableEditListener( new UndoableEditListener() {
            public void undoableEditHappened( UndoableEditEvent e ) {
                //Remember the edit and update the menus
                undo.addEdit( e.getEdit() );
                undoAction.update();
                redoAction.update();
            }
        } );
	}

}