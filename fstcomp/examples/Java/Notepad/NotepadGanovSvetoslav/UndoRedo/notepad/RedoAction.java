package notepad;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.undo.CannotRedoException;

class RedoAction extends AbstractAction{
	public static final long serialVersionUID = 1L;

	Notepad notepad;

	public RedoAction(Notepad notepad){
		super("Redo");
		putValue( Action.SMALL_ICON, 
				new ImageIcon(this.getClass().getResource("images/redo.gif")));
		setEnabled(false);
		this.notepad = notepad;
	}
	public void actionPerformed(ActionEvent e){
		try{
			notepad.getUndo().redo();
		}
		catch (CannotRedoException ex){
			System.out.println("Unable to redo: " + ex);
			ex.printStackTrace();
		}
		update();
		notepad.getUndoAction().update();
	}
	protected void update(){
		if(notepad.getUndo().canRedo()){
			setEnabled(true);
			putValue("Redo", notepad.getUndo().getRedoPresentationName());
		}
		else{
			setEnabled(false);
			putValue(Action.NAME, "Redo");
		}
	}
}
