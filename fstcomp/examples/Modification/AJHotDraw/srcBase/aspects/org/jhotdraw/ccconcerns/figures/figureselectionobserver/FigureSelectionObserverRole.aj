package org.jhotdraw.ccconcerns.figures.figureselectionobserver;

import org.jhotdraw.framework.FigureSelectionListener;
import org.jhotdraw.standard.AbstractCommand;
import org.jhotdraw.framework.DrawingEditor;
//import org.jhotdraw.util.UndoableCommand;
import org.jhotdraw.applet.DrawApplet;
import org.jhotdraw.application.DrawApplication;
import org.jhotdraw.ccconcerns.GenericRole;
import org.jhotdraw.samples.javadraw.JavaDrawViewer;
import org.jhotdraw.framework.DrawingView;

/**
 * @author Marius Marin
 * 
 * Defines the observer role for the observers of the figures'
 * selection.
 * Some of the methods accessed by the observer's update method
 * (figureSelectionChanged), such as checkCommandMenus, are not
 * visible, hence the aspect is declared privileged.
 */

public privileged aspect FigureSelectionObserverRole {

    /*
     * The FigureSelectionListener would be better declared in this aspect
     * and not in the framework package. However, for the purposes
     * of conserving the interface an having the test project 
     * running uniformly on both the original and refactored version,
     * we leave the FigureSelectionListener interace in its original
     * package.
     */
    declare parents: FigureSelectionListener extends GenericRole;
    
    declare parents: AbstractCommand implements FigureSelectionListener;
//    declare parents: UndoableCommand implements FigureSelectionListener;
    declare parents: DrawingEditor extends FigureSelectionListener;
    declare parents: DrawApplet implements FigureSelectionListener;
    declare parents: DrawApplication implements FigureSelectionListener;
    declare parents: JavaDrawViewer implements FigureSelectionListener;
    
    
	/**
	 * @param view a DrawingView
	 */
	public void AbstractCommand.figureSelectionChanged(DrawingView view) {
	}
    
//	public void UndoableCommand.figureSelectionChanged(DrawingView view) {
//	    hasSelectionChanged = true;
//	}

	/**
	 * Handles a change of the current selection. Updates all
	 * menu items that are selection sensitive.
	 * @see DrawingEditor
	 */
	public void DrawApplet.figureSelectionChanged(DrawingView view) {
		setupAttributes();
	}
	
	/**
	 * Fired by a view when the figure selection changes.  Since Commands and
	 * Tools may depend on the figure selection they are registered to be notified
	 * about these events.
	 * Any selection sensitive GUI component should update its
	 * own state if the selection has changed, e.g. selection sensitive menuitems
	 * will update their own states.
	 * @see DrawingEditor
	 */
	public void DrawApplication.figureSelectionChanged(DrawingView view) {
		checkCommandMenus();
	}

	/**
	 * Ignore selection changes, we don't show any selection
	 */
	public void JavaDrawViewer.figureSelectionChanged(DrawingView view) {}

}
