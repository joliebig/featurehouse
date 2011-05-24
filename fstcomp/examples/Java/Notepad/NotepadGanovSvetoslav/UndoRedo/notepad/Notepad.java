package notepad;

import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.undo.UndoManager;

class Notepad {	
	//fields 
	private UndoManager undo = new UndoManager();
	private UndoAction undoAction = new UndoAction(this);
	private RedoAction redoAction = new RedoAction(this);
	
	//initialization
	Notepad() {
		//MENU
		if (ediT == null) {
			ediT = new JMenu("Edit");
		}
		//undo
		ediT.add(undoAction);
		//redo
		ediT.add(redoAction);
		ediT.addSeparator();
		menubar.add(ediT);
		//TOOLBAR
		//undo
		toolBar.add(undoAction);
		//redo
		toolBar.add(redoAction);
		toolBar.addSeparator();
		//button/menu listeners
		textArea.getDocument().addUndoableEditListener(new UndoableEditListener(){
			public void undoableEditHappened(UndoableEditEvent e){
				//Remember the edit and update the menus
				undo.addEdit(e.getEdit());
				undoAction.update();
				redoAction.update();
			}
		}); 
	}
	
	public UndoManager getUndo() {
		return undo;
	}
	
	public RedoAction getRedoAction() {
		return redoAction;
	}
	public UndoAction getUndoAction() {
		return undoAction;
	}
}			
