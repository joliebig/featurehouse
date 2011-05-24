package org.jhotdraw.ccconcerns.figures.figureselectionobserver;

import org.jhotdraw.ccconcerns.GenericRole;
import org.jhotdraw.framework.FigureSelectionListener;
import org.jhotdraw.framework.DrawingView;
import org.jhotdraw.framework.DrawingEditor;
import org.jhotdraw.standard.StandardDrawingView;
import org.jhotdraw.standard.NullDrawingView;
import org.jhotdraw.util.CollectionsFactory;

import java.util.List;
import java.io.ObjectInputStream;

/**
 * @author Marius Marin
 *
 * Note: the notifier is not part of the interface in the 
 * original design. 
 * Adding the notifier will change the interface of the original
 * app. The notifier is present only in the concrete subject
 * (StandardDrawingView) - this is a "deviated" implementation of
 * the pattern.
 * 
 */
public privileged aspect FigureSelectionSubjectRole {

	/**
	 * the registered listeners for selection changes
	 * TODO field in StandardDrawingView vs. FigureSelectionSubject
	 */
	private transient List StandardDrawingView.fSelectionListeners;

	// initialize the listeners and add the editor as a listener
	after(StandardDrawingView sdv): 
        initialization(StandardDrawingView.new(DrawingEditor, int , int))
		&& this(sdv) {

		sdv.fSelectionListeners = CollectionsFactory.current().createList();

		sdv.addFigureSelectionListener(sdv.editor());
	}

	//reset the listeners list after reading an object
	after(StandardDrawingView sdv): 
        execution(void StandardDrawingView.readObject(ObjectInputStream)) 
        && this(sdv) {
		sdv.fSelectionListeners = CollectionsFactory.current().createList();
	}

	/**
	 * define the subject role for figure selection
	 */
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

	declare parents: DrawingView extends FigureSelectionSubject;

	/**
	 * Add a listener for selection changes.
	 * 
	 * @param fsl
	 *            jhotdraw.framework.FigureSelectionListener
	 */
	public void StandardDrawingView.addFigureSelectionListener(
			FigureSelectionListener fsl) {
		fSelectionListeners.add(fsl);
	}

	/**
	 * Remove a listener for selection changes.
	 * 
	 * @param fsl
	 *            jhotdraw.framework.FigureSelectionListener
	 */
	public void StandardDrawingView.removeFigureSelectionListener(
			FigureSelectionListener fsl) {
		fSelectionListeners.remove(fsl);
	}

	/**
	 * Informs that the current selection changed. By default this event is
	 * forwarded to the drawing editor.
	 * 
	 * AJHD the visibility has to be modified
	 */
	public void StandardDrawingView.fireSelectionChanged() {
		if (fSelectionListeners != null) {
			for (int i = 0; i < fSelectionListeners.size(); i++) {
				FigureSelectionListener l = (FigureSelectionListener) fSelectionListeners
						.get(i);
				l.figureSelectionChanged(this);
			}
		}
	}

	// ---------- NullDrawingView ----------

	/**
	 * Add a listener for selection changes in this DrawingView.
	 * 
	 * @param fsl
	 *            jhotdraw.framework.FigureSelectionListener
	 */
	public void NullDrawingView.addFigureSelectionListener(
			FigureSelectionListener fsl) {
		// ignore: do nothing
	}

	/**
	 * Remove a listener for selection changes in this DrawingView.
	 * 
	 * @param fsl
	 *            jhotdraw.framework.FigureSelectionListener
	 */
	public void NullDrawingView.removeFigureSelectionListener(
			FigureSelectionListener fsl) {
		// ignore: do nothing
	}
}
