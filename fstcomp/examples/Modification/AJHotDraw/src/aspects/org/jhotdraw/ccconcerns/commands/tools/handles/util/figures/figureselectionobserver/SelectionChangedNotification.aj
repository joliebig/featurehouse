package org.jhotdraw.ccconcerns.figures.figureselectionobserver;

import org.jhotdraw.framework.Figure;
import org.jhotdraw.standard.StandardDrawingView;

/**
 * @author Marius Marin
 *
 * Implements the consistent behavior imposed to the methods
 * changing the selection: call the notification method
 * of the subject role in the context of the Observer pattern. 
 * There is a deviation however, from the standard implementation
 * of the pattern, as the notifier method is defined only for the 
 * concrete subject.
 * 
 * This refactoring is based on not so obvious observations
 * of the original code behavior: the methods notifing selection changes
 * perform this operation (if not always then) only if they (1) modify 
 * the list of currently selected figures (fSelection) or
 * (2) a figure.invalidate() is sent. 
 * Unfortunately for the first (1), more consistent condition, 
 * fSelection is a 
 * java.util.List and AspectJ does not capture changes in these 
 * structures (such as adding/removing elements).
 * 
 * The solution here relies on the second(2) observed behavior.
 *  
 * This refactoring suffer from several <p>drawbacks</p>: 
 * - need to understand how the methods calling the notifier work internally
 * - modifications of the logic of these methods might affect the functionality 
 *   of the aspect without warning (for instance, removing the call to figure.invalidate())
 * 
 */
public aspect SelectionChangedNotification {

    /*
     * This pointcut relies on the fact that the calls to 
     * addToSelectionImpl are only from StandardDrawingView.addToSelection 
     * and StandardDrawingView.addToSelectionAll
     * and then followed by a call to the notifier. 
     * It's not a clean design
     * but it tries to avoid the mentioned AspectJ shortcomings.
     *  
     */
    pointcut invalidateSelFigure(StandardDrawingView sdw) :
        (   withincode(boolean StandardDrawingView.addToSelectionImpl(Figure)) 
         || withincode(void StandardDrawingView.removeFromSelection(Figure)))
        && call(void Figure.invalidate()) 
        && this(sdw);
    
    pointcut clear_toggleSelection(StandardDrawingView sdw):
        (execution(void StandardDrawingView.clearSelection()) ||
         execution(void StandardDrawingView.toggleSelection(Figure)))
        && this(sdw);

    /**
     * Call the notifier and not the updaters because this
     * aspect does not have to know that the Subject has further
     * observers (i.e. that the subject is a Subject). 
     */
    after(StandardDrawingView sdw): invalidateSelFigure(sdw) {
        sdw.fireSelectionChanged();
    }
    
    after(StandardDrawingView sdw): clear_toggleSelection(sdw) {
        sdw.fireSelectionChanged();
    }
    
    
// ********** Partial implementation of the refactoring based 
// ********** on reproducing the immediate logic/flow of the refactored code
// ********** The main difference from the previous solution is that that
// ********** one is relatively easier to write.
  
//    protected pointcut selectionChange(StandardDrawingView s): 
//        call( 
////              void StandardDrawingView.addToSelection(Figure) ||
////              void StandardDrawingView.addToSelectionAll(FigureEnumeration) ||
//              void StandardDrawingView.clearSelection() ||
////              void StandardDrawingView.removeFromSelection(Figure) ||
//              void StandardDrawingView.toggleSelection(Figure)
//             ) 
//        && target(s);
//
//    /**
//     * Call the notifier and not the updaters because it is not
//     * the buisiness of this concern to know that the subject has
//     * observers (i.e. that the subject is a subject). 
//     * The crosscutting occurs at the role level and the organization
//     * on roles, according to a pattern, is a design decision.
//     */
//    after(StandardDrawingView s): selectionChange(s) {
//        s.fireSelectionChanged();
//    }
//    
//    //----------------------------------------------------------
//    //define whatchers for the methods that change the selection,
//    //and notify the changes if given conditions in these methods hold
//    
//    //1. StandardDrawingView.addToSelection(Figure)
//    /**
//     * Variable that is set to the same value as the local (in this case, virtual)
//     * variable "changed" in StandardDrawingView.addToSelection(Figure).
//     * It retains the value returned by the call to addToSelectionImpl(..).
//     * The notification method is called only if this variable is set
//     * to "true".
//     */
//    private boolean selectionChanged = false; 
//    
//    /*
//     * Capture the call to addToSelectionImpl from addToSelection(..)
//     * The notification call (to fireSelectionChanged) is conditioned by the 
//     * value returned by the call captured by this pointcut.
//     * TODO: It is not too great that the aspect relies on the internal 
//     * structure of the method to be advised - the aspect "knows" what's inside
//     * the advised method: what and how it does it does. 
//     * Modifications to that internal structure will affect my pointcut,
//     * and thus the logic in the aspect!
//     */
//    private pointcut sdwcall_addToSelectionImpl():
//		call(boolean StandardDrawingView.addToSelectionImpl(Figure))
//		&& withincode(void StandardDrawingView.addToSelection(Figure));
//
//	after() returning(boolean changed) : 
//	    sdwcall_addToSelectionImpl() {
//			selectionChanged = changed;
//	}
//	
//	/*
//	 * This advice could be merged with the previous one,
//	 * but they are different concerns and that's why they are kept
//	 * separated. 
//	 */
//	after(StandardDrawingView s): 
//	    call (void StandardDrawingView.addToSelection(Figure)) &&
//	    target (s) {
//	        if(selectionChanged == true)	    
//                s.fireSelectionChanged();
//    }
//
//    //2. StandardDrawingView.removeFromSelection(Figure)
//    
//	
//	//3. StandardDrawingView.addToSelectionAll(FigureEnumeration)
	
}

