	public static class UndoActivity extends UndoableAdapter {

		private FigureTransferCommand myCommand;
		private List mySelectedFigures;

		/**
		 * Constructor for <code>UndoActivity</code>.
		 * @param newCommand
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
				myCommand.insertFigures(getAffectedFiguresReversed(), 0, 0);
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
				myCommand.copyFigures(getSelectedFigures(), getSelectedFiguresCount());
				myCommand.deleteFigures(getAffectedFigures());
				return true;
			}

			return false;
		}

		/**
		 * Preserve the selection of figures the moment the command was executed.
		 * @param newSelectedFigures
		 */
		public void setSelectedFigures(FigureEnumeration newSelectedFigures) {
			// the enumeration is not reusable therefore a copy is made
			// to be able to undo-redo the command several time
			rememberSelectedFigures(newSelectedFigures);
		}

		/**
		 * Preserve a copy of the enumeration in a private list.
		 * @param toBeRemembered
		 */
		protected void rememberSelectedFigures(FigureEnumeration toBeRemembered) {
			mySelectedFigures = CollectionsFactory.current().createList();
			while (toBeRemembered.hasNextFigure()) {
				mySelectedFigures.add(toBeRemembered.nextFigure());
			}
		}
	
		/**
		 * Returns the selection of figures to perform the command on.
		 * @return
		 */
		public FigureEnumeration getSelectedFigures() {
			return new FigureEnumerator(
				CollectionsFactory.current().createList(mySelectedFigures));
		}

		/**
		 * Returns the size of the selection.
		 * @return
		 */
		public int getSelectedFiguresCount() {
			return mySelectedFigures.size();
		}

		/**
		 * @see org.jhotdraw.util.UndoableAdapter#release()
		 */
		public void release() {
			super.release();
			FigureEnumeration fe = getSelectedFigures();
			while (fe.hasNextFigure()) {
				fe.nextFigure().release();
			}
			setSelectedFigures(FigureEnumerator.getEmptyEnumeration());
		}
	}

