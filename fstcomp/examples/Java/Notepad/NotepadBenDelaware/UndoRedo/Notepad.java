

/**
 *A PUBLIC CLASS FOR NOTEPAD.JAVA
 */
class Notepad {

	//for using the methods in these classes
	//for using undo & redo
	UndoManager undo = new UndoManager();
	UndoAction undoAction = new UndoAction(this);
	RedoAction redoAction = new RedoAction(this);
	
	//declaration of the private variables used in the program
	//Create the menu items
     JButton undoButton, redoButton;

    void MenuRedoHook(){

}

   void ToolBarRedoHook(){

}

    //Constructor of Notepad
    Notepad(){
		/**
		 *adding undO & redO to the ediT Menu,
		 *adding a small image icon to the menu item &
		 *adding separator between the menu item
		 */
		ediT.add(undoAction);
		ediT.add(redoAction);
		
		toolBar.add(undoAction);
		toolBar.add(redoAction);
		toolBar.addSeparator();

		textArea.getDocument().addUndoableEditListener(new UndoableEditListener(){
			public void undoableEditHappened(UndoableEditEvent e){
				//Remember the edit and update the menus
				undo.addEdit(e.getEdit());
				undoAction.update();
				redoAction.update();
			}
		}); 
		
	}
}			
