
import javax.swing.undo.*;

class Notepad {

//     //for using undo & redo
    UndoManager undo = new UndoManager();
    UndoAction undoAction;
    RedoAction redoAction;



    void initMenuItems() {
        
    	original();
        
        ediT.addSeparator();
        ediT.add(undoAction);
        ediT.add(redoAction);
        

    }

    void initToolBar() {

    	original();
        
        toolBar.addSeparator();
        toolBar.add(undoAction);
        toolBar.add(redoAction);


    }

    void initActionListeners() {

    	original();

        textArea.getDocument().addUndoableEditListener(new UndoableEditListener(){
                public void undoableEditHappened(UndoableEditEvent e){
                    //Remember the edit and update the menus
                    undo.addEdit(e.getEdit());
                    undoAction.update();
                    redoAction.update();

                }
            }); 

    }

    void setup() {

    	original();

        undoAction = new UndoAction(this);
        redoAction = new RedoAction(this);
        
    }

}
