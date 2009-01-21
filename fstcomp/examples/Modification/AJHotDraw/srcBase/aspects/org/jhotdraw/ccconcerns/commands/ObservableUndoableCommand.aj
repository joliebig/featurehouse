package org.jhotdraw.ccconcerns.commands;


/**
 * 
 * Observable role for UndoableCommand - this type does not extend
 * AbstractCommand, and it is, in fact, a Command wrapper.
 * 
 * The redirector will now take care of this since the receiver (AbstractCommand) declares the 
 * role-methods itself, and the multi-role UndoableCommand has been replaced by an aspect.
 * 
 * @author Marius Marin
 * 
 */
public aspect ObservableUndoableCommand {

//	private CommandObserver.EventDispatcher UndoableCommand.myEventDispatcher;
//
//	public void UndoableCommand.addCommandListener(CommandListener newCommandListener) {
//		getEventDispatcher().addCommandListener(newCommandListener);
//	}
//
//	public void UndoableCommand.removeCommandListener(CommandListener oldCommandListener) {
//		getEventDispatcher().removeCommandListener(oldCommandListener);
//	}
//
//	private void UndoableCommand.setEventDispatcher(/*AbstractCommand*/CommandObserver.EventDispatcher newEventDispatcher) {
//		myEventDispatcher = newEventDispatcher;
//	}
//
//	/*protected*/public /*AbstractCommand*/CommandObserver.EventDispatcher UndoableCommand.getEventDispatcher() {
//		return myEventDispatcher;
//	}
//
//	public /*AbstractCommand*/CommandObserver.EventDispatcher UndoableCommand.createEventDispatcher() {
//		return new /*AbstractCommand*/CommandObserver.EventDispatcher(this);
//	}
//
//	
//	//CB: init the command event dispatcher in the AbstractCommand constructor
//	after(UndoableCommand command) : this(command) && execution(UndoableCommand.new(Command)) {
//		command.setEventDispatcher(command.createEventDispatcher());
//	}

}
