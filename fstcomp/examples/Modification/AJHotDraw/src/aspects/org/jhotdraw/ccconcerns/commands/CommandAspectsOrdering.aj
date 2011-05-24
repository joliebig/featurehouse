package org.jhotdraw.ccconcerns.commands;


/**
 * Takes care of the order of the aspects for cases like where more
 * advices are attached to the same joinpoint.
 * 
 * 
 * @author marin
 */
public aspect CommandAspectsOrdering {

	//precedence for the two after advices attached to the UndoableCommand constructor
	//!!! This order actually means that the *after* advice in ObservableUndoableCommand will 
	//excute second; that is, this advice is more of an "after" advice than the other one.
	declare precedence: UndoableCommand, ObservableUndoableCommand, CommandObserver;
}

