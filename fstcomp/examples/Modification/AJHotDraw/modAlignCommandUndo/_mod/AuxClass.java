	public static class UndoActivity extends UndoableAdapter {
		private Hashtable myOriginalPoints;
		private Alignment myAppliedAlignment;

		public UndoActivity(DrawingView newView, Alignment newAlignment) {
			super(newView);
			myOriginalPoints = new Hashtable();
			setAppliedAlignment(newAlignment);
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
				Point originalPoint = getOriginalPoint(f);
				Point currentPoint = f.displayBox().getLocation();
				// substract current lcoation to get to 0,0 and then move to original location
				f.moveBy(-currentPoint.x + originalPoint.x,
						 -currentPoint.y + originalPoint.y);
			}

			return true;
		}

		public boolean redo() {
			if (!isRedoable()) {
				return false;
			}
			alignAffectedFigures(getAppliedAlignment());
			return true;
		}

		protected void setAppliedAlignment(Alignment newAlignment) {
			myAppliedAlignment = newAlignment;
		}

		public Alignment getAppliedAlignment() {
			return myAppliedAlignment;
		}

		protected void addOriginalPoint(Figure f) {
			myOriginalPoints.put(f, f.displayBox().getLocation());
		}

		public Point getOriginalPoint(Figure f) {
			return (Point)myOriginalPoints.get(f);
		}

		public void alignAffectedFigures(Alignment applyAlignment) {
			FigureEnumeration fe = getAffectedFigures();
			Figure anchorFigure = fe.nextFigure();
			Rectangle r = anchorFigure.displayBox();

			while (fe.hasNextFigure()) {
				Figure f = fe.nextFigure();
				applyAlignment.moveBy(f, r);
			}
		}

		public void setAffectedFigures(FigureEnumeration fe) {
			// first make copy of FigureEnumeration in superclass
			super.setAffectedFigures(fe);
			// then get new FigureEnumeration of copy to save aligment
			FigureEnumeration copyFe = getAffectedFigures();
			while (copyFe.hasNextFigure()) {
				addOriginalPoint(copyFe.nextFigure());
			}
		}
	}

