public class AuxClass {

    public static class UndoActivity extends UndoableAdapter {

	private FigureTransferCommand myCommand;

	/**
	 * Constructor for <code>UndoActivity</code>.
	 * 
	 * @param newCommand
	 *            parent command
	 */
	public UndoActivity(FigureTransferCommand newCommand) {
	    super(newCommand.view());
	    myCommand = newCommand;
	    setUndoable(true);
	    setRedoable(true);
	}

	/**
	 * @see org.jhotdraw.util.Undoable#undo()
	 */
	public boolean undo() {
	    if (super.undo() && getAffectedFigures().hasNextFigure()) {
		getDrawingView().clearSelection();
		setAffectedFigures(myCommand.insertFigures(
			getAffectedFiguresReversed(), 0, 0));
		return true;
	    }
	    return false;
	}

	/**
	 * @see org.jhotdraw.util.Undoable#redo()
	 */
	public boolean redo() {
	    // do not call execute directly as the selection might has changed
	    if (isRedoable()) {
		myCommand.deleteFigures(getAffectedFigures());
		getDrawingView().clearSelection();
		return true;
	    }
	    return false;
	}
    }
}
