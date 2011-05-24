/*
 * @(#)UndoableCommand.java
 *
 * Project:		JHotdraw - a GUI framework for technical drawings
 *				http://www.jhotdraw.org
 *				http://jhotdraw.sourceforge.net
 * Copyright:	� by the original author(s) and all contributors
 * License:		Lesser GNU Public License (LGPL)
 *				http://www.opensource.org/licenses/lgpl-license.html
 */

//@AJHD refactored as a RL instance

//package org.jhotdraw.util;
//
//import org.jhotdraw.framework.*;
//import org.jhotdraw.standard.AbstractCommand;
//import java.util.EventObject;
//

///**
// * @author  Wolfram Kaiser <mrfloppy@sourceforge.net>
// * @version <$CURRENT_VERSION$>
// * 
// * @AJHD refactored: @author marin
// * RSI: listener for Figure selection; listener for Command events 
// * CB: registration of Command listeners, init/set event dispatcher etc
// *  
// */
//public class UndoableCommand implements Command 
//	/*@AJHD refactored FigureSelectionListener,*/ 
//	/*@AJHD refactored CommandListener*/ {
//
//	private Command myWrappedCommand;
//	private boolean hasSelectionChanged;
////	@AJHD refactored - The ObservableCommand role
////	private AbstractCommand.EventDispatcher myEventDispatcher;
//
//	public UndoableCommand(Command newWrappedCommand) {
//		setWrappedCommand(newWrappedCommand);
////		@AJHD refactored
////		getWrappedCommand().addCommandListener(this);
////		@AJHD refactored
////		setEventDispatcher(createEventDispatcher());
//	}
//
//	/**
//	 * Executes the command.
//	 */
//	public void execute() {
//		hasSelectionChanged = false;
//		// listen for selection change events during executing the wrapped command
//		view().addFigureSelectionListener(this);
//
//		getWrappedCommand().execute();
//
//		Undoable undoableCommand = getWrappedCommand().getUndoActivity();
//		if ((undoableCommand != null) && (undoableCommand.isUndoable())) {
//			getDrawingEditor().getUndoManager().pushUndo(undoableCommand);
//			getDrawingEditor().getUndoManager().clearRedos();
//		}
//
//		// initiate manual update of undo/redo menu states if it has not
//		// been done automatically during executing the wrapped command
//		if (!hasSelectionChanged || (getDrawingEditor().getUndoManager().getUndoSize() == 1)) {
//			getDrawingEditor().figureSelectionChanged(view());
//		}
//
//		// remove because not all commands are listeners that have to be notified
//		// all the time (bug-id 595461)
//		view().removeFigureSelectionListener(this);
//	}
//
//	/**
//	 * Tests if the command can be executed.
//	 */
//	public boolean isExecutable() {
//		return getWrappedCommand().isExecutable();
//	}
//
//	protected void setWrappedCommand(Command newWrappedCommand) {
//		myWrappedCommand = newWrappedCommand;
//	}
//
//	//@AJHD refactored - increased the visibility of the method 
//	//to call it in the aspects from the org.jhotdraw.ccconcerns.commands.* package
//	/*@AJHD protected*/public Command getWrappedCommand() {
//		return myWrappedCommand;
//	}
//
//	/**
//	 * Gets the command name.
//	 */
//	public String name() {
//		return getWrappedCommand().name();
//	}
//
//	public DrawingEditor getDrawingEditor() {
//		return getWrappedCommand().getDrawingEditor();
//	}
//
//	public DrawingView view() {
//		return getDrawingEditor().view();
//	}
//
////	@AJHD refactored: role element
////	
////	public void figureSelectionChanged(DrawingView view) {
////		hasSelectionChanged = true;
////	}
//
//	public Undoable getUndoActivity() {
//		return new UndoableAdapter(view());
//	}
//
//	public void setUndoActivity(Undoable newUndoableActivity) {
//		// do nothing: always return default UndoableAdapter
//	}
//
////	@AJHD refactored - The ObservableUndoableCommand role
////	public void addCommandListener(CommandListener newCommandListener) {
////		getEventDispatcher().addCommandListener(newCommandListener);
////	}
////
////	@AJHD refactored
////	public void removeCommandListener(CommandListener oldCommandListener) {
////		getEventDispatcher().removeCommandListener(oldCommandListener);
////	}
////
////	@AJHD refactored
////	private void setEventDispatcher(AbstractCommand.EventDispatcher newEventDispatcher) {
////		myEventDispatcher = newEventDispatcher;
////	}
////
////	@AJHD refactored
////	protected AbstractCommand.EventDispatcher getEventDispatcher() {
////		return myEventDispatcher;
////	}
////
////	@AJHD refactored
////	public AbstractCommand.EventDispatcher createEventDispatcher() {
////		return new AbstractCommand.EventDispatcher(this);
////	}
//
////	@AJHD refactored
////	public void commandExecuted(EventObject commandEvent) {
////		getEventDispatcher().fireCommandExecutedEvent();
////	}
////
////	@AJHD refactored
////	public void commandExecutable(EventObject commandEvent) {
////		getEventDispatcher().fireCommandExecutableEvent();
////	}
////
////	@AJHD refactored
////	public void commandNotExecutable(EventObject commandEvent) {
////		getEventDispatcher().fireCommandNotExecutableEvent();
////	}
//}
