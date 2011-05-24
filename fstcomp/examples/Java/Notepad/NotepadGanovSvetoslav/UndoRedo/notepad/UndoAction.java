package notepad;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.undo.CannotUndoException;

class UndoAction extends AbstractAction {
	public static final long serialVersionUID = 1L;

	Notepad notepad;

	public UndoAction(Notepad notepad){
		super( "Undo" );
		putValue( Action.SMALL_ICON, 
				new ImageIcon( this.getClass().getResource( "images/undo.gif" ) ) );
		setEnabled( false );
		this.notepad = notepad;
	}
	public void actionPerformed( ActionEvent e ) {
		try {
			notepad.getUndo().undo();
		}
		catch ( CannotUndoException ex ) {
			System.out.println( "Unable to undo: " + ex );
			ex.printStackTrace();
		}
		update();
		notepad.getRedoAction().update();
	}
	protected void update() {
		if( notepad.getUndo().canUndo() ) {
			setEnabled( true );
			putValue( "Undo", notepad.getUndo().getUndoPresentationName() );
		}
		else {
			setEnabled( false );
			putValue( Action.NAME, "Undo" );
		}
	}
}