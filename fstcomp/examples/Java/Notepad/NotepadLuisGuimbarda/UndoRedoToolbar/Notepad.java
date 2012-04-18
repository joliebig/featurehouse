

class Notepad {
    //for using undo & redo
    UndoManager undo = new UndoManager();
    UndoAction undoAction = new UndoAction(this);
    RedoAction redoAction = new RedoAction(this);
    private JButton undoButton, redoButton;

    Notepad(){
        /**
         *adding undO, redO &
         *adding a small image icon to the menu item
         */
        ediT.add(undoAction);
        ediT.add(redoAction);
        /**
         *adding undoButton, redoButton to the tool bar &
         *adding a small image icon to the menu item
         */
        toolBar.add(undoAction);
        toolBar.add(redoAction);

        textArea.getDocument().addUndoableEditListener(new UndoableEditListener(){
            public void undoableEditHappened(UndoableEditEvent e){
                //Remember the edit and update the menus
                undo.addEdit(e.getEdit());
                undoAction.update();
                redoAction.update();
            }
        });

    }
    private void conditionalAddToolbar(Container cp){
        cp.add("North", toolBar = new JToolBar("Tool Bar"));
    }

}
