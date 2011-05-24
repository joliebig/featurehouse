	public interface FigureSelectionSubject extends GenericRole {

		/**
		 * Add a listener for selection changes in this DrawingView.
		 * 
		 * @param fsl
		 *            jhotdraw.framework.FigureSelectionListener
		 */
		public void addFigureSelectionListener(FigureSelectionListener fsl);

		/**
		 * Remove a listener for selection changes in this DrawingView.
		 * 
		 * @param fsl
		 *            jhotdraw.framework.FigureSelectionListener
		 */
		public void removeFigureSelectionListener(FigureSelectionListener fsl);

	}
