	public static class UndoActivity extends UndoableAdapter {
		private FigureAttributeConstant myUndoAttribute;
		private Hashtable	            myOriginalValues;
		private Object                  myUndoValue;

		public UndoActivity(DrawingView newDrawingView, FigureAttributeConstant newUndoAttribute, Object newUndoValue) {
			super(newDrawingView);
			myOriginalValues = new Hashtable();
			setAttribute(newUndoAttribute);
			setBackupValue(newUndoValue);
			setUndoable(true);
			setRedoable(true);
		}

		public boolean undo() {
			if (!super.undo()) {
				return false;
			}

			FigureEnumeration fe = getAffectedFigures();
			while (fe.hasNextFigure()) {
				Figure f = fe.nextFigure();
				if (getOriginalValue(f) != null) {
					f.setAttribute(getAttribute(), getOriginalValue(f));
				}
			}

			return true;
		}

		public boolean redo() {
			if (!isRedoable()) {
				return false;
			}

			FigureEnumeration fe = getAffectedFigures();
			while (fe.hasNextFigure()) {
				Figure f = fe.nextFigure();
				if (getBackupValue() != null) {
					f.setAttribute(getAttribute(), getBackupValue());
				}
			}

			return true;
		}

		protected void addOriginalValue(Figure affectedFigure, Object newOriginalValue) {
			myOriginalValues.put(affectedFigure, newOriginalValue);
		}

		protected Object getOriginalValue(Figure lookupAffectedFigure) {
			return myOriginalValues.get(lookupAffectedFigure);
		}

		protected void setAttribute(FigureAttributeConstant newUndoAttribute) {
			myUndoAttribute = newUndoAttribute;
		}

		public FigureAttributeConstant getAttribute() {
			return myUndoAttribute;
		}

		protected void setBackupValue(Object newUndoValue) {
			myUndoValue = newUndoValue;
		}

		public Object getBackupValue() {
			return myUndoValue;
		}

		public void release() {
			super.release();
			myOriginalValues = null;
		}

		public void setAffectedFigures(FigureEnumeration fe) {
			// first make copy of FigureEnumeration in superclass
			super.setAffectedFigures(fe);
			// then get new FigureEnumeration of copy to save attributes
			FigureEnumeration copyFe = getAffectedFigures();
			while (copyFe.hasNextFigure()) {
				Figure f = copyFe.nextFigure();
				Object attributeValue = f.getAttribute(getAttribute());
				if (attributeValue != null) {
					addOriginalValue(f, attributeValue);
				}
			}
		}
	}
