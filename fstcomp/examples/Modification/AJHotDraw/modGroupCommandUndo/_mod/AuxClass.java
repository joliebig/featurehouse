	public static class UndoActivity extends UndoableAdapter {
		public UndoActivity(DrawingView newDrawingView) {
			super(newDrawingView);
			setUndoable(true);
			setRedoable(true);
		}

		public boolean undo() {
			if (!super.undo()) {
				return false;
			}

			getDrawingView().clearSelection();

			// orphan group figure(s)
			getDrawingView().drawing().orphanAll(getAffectedFigures());

			// create a new collection with the grouped figures as elements
			List affectedFigures = CollectionsFactory.current().createList();

			FigureEnumeration fe = getAffectedFigures();
			while (fe.hasNextFigure()) {
				Figure currentFigure = fe.nextFigure();
				// add contained figures
				getDrawingView().drawing().addAll(currentFigure.figures());
				getDrawingView().addToSelectionAll(currentFigure.figures());

				FigureEnumeration groupedFigures = currentFigure.figures();
				while (groupedFigures.hasNextFigure()) {
					affectedFigures.add(groupedFigures.nextFigure());
				}
			}

			setAffectedFigures(new FigureEnumerator(affectedFigures));

			return true;
		}

		public boolean redo() {
			// do not call execute directly as the selection might has changed
			if (isRedoable()) {
				groupFigures();
				return true;
			}

			return false;
		}

		public void groupFigures() {
			getDrawingView().drawing().orphanAll(getAffectedFigures());
			getDrawingView().clearSelection();

			// add new group figure instead
			GroupFigure group = new GroupFigure();
			group.addAll(getAffectedFigures());

			Figure figure = getDrawingView().drawing().add(group);
			getDrawingView().addToSelection(figure);

			// create a new collection with the new group figure as element
			List affectedFigures = CollectionsFactory.current().createList();
			affectedFigures.add(figure);
			setAffectedFigures(new FigureEnumerator(affectedFigures));
		}
	}

