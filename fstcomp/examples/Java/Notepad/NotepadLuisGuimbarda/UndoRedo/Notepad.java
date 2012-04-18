

class Notepad {
    //for using undo & redo
    UndoManager undo = new UndoManager();
    UndoAction undoAction = new UndoAction(this);
    RedoAction redoAction = new RedoAction(this);

    Notepad(){
        /**
         *adding undO, redO &
         *adding a small image icon to the menu item
         */
        ediT.add(undoAction);
        ediT.add(redoAction);

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
