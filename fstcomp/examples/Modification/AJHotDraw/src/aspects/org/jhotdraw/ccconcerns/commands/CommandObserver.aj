package org.jhotdraw.ccconcerns.commands;

import java.util.EventObject;
import java.util.Iterator;
import java.util.List;
import javax.swing.JMenuItem;

import org.jhotdraw.ccconcerns.GenericRole;
import org.jhotdraw.contrib.CommandHolder;
import org.jhotdraw.contrib.CTXCommandMenu;
import org.jhotdraw.framework.DrawingEditor;
import org.jhotdraw.framework.DrawingView;
import org.jhotdraw.standard.AbstractCommand;
import org.jhotdraw.util.CollectionsFactory;
import org.jhotdraw.util.Command;
import org.jhotdraw.util.CommandListener;
import org.jhotdraw.util.CommandMenu;
//import org.jhotdraw.util.UndoableCommand;

/**
 * Aspect refactoring of the Observer design for Commands.
 * The design implementation comprises a number of crosscutting concerns/sort instances, like:
 * - RSI: The Observable role for Commands (ObservableComamnd)
 * - RSI: The Listener role (CommandListener)
 * - CB: various instances for notification of various events in commands, like "command executed"
 * - SC: the notification dispatcher for the notification of events (EventDispatcher)
 * - ...   
 * 
 * TODO Move the various concerns to different files
 * 
 * @author Marius Marin
 */
public aspect CommandObserver {
	//ObservableCommand role
	
	/**
	 * define the subject role for figure selection
	 */    
    public interface ObservableCommand extends GenericRole {
    	public void addCommandListener(CommandListener newCommandListener);
    	public void removeCommandListener(CommandListener oldCommandListener);
    }

	declare parents: Command extends ObservableCommand;
    
    //role members in AbstractCommand
	public void AbstractCommand.addCommandListener(CommandListener newCommandListener) {
		getEventDispatcher().addCommandListener(newCommandListener);
	}
	
	public void AbstractCommand.removeCommandListener(CommandListener oldCommandListener) {
		getEventDispatcher().removeCommandListener(oldCommandListener);
	}

	private CommandObserver.EventDispatcher AbstractCommand.myEventDispatcher;
	
	private void AbstractCommand.setEventDispatcher(CommandObserver.EventDispatcher newEventDispatcher) {
		myEventDispatcher = newEventDispatcher;
	}

	/*protected*/public CommandObserver.EventDispatcher AbstractCommand.getEventDispatcher() {
		return myEventDispatcher;
	}

	/*protected*/public CommandObserver.EventDispatcher AbstractCommand.createEventDispatcher() {
		return new CommandObserver.EventDispatcher(this);
	}

	
	//command listener role
	declare parents: CommandMenu implements CommandListener;
	declare parents: CTXCommandMenu implements CommandListener;
//	declare parents: UndoableCommand implements CommandListener;
	
	public void CommandMenu.commandExecuted(EventObject commandEvent) {
		//checkEnabled();
	}

	public void CommandMenu.commandExecutable(EventObject commandEvent) {
		//checkEnabled();
	}

	public void CommandMenu.commandNotExecutable(EventObject commandEvent) {
		//checkEnabled();
	}
	

	/**
	 * Description of the Method
	 *
	 * @param commandEvent  Description of the Parameter
	 */
	public void CTXCommandMenu.commandExecuted(EventObject commandEvent) {
//		checkEnabled();
	}

	/**
	 * Description of the Method
	 *
	 * @param commandEvent  Description of the Parameter
	 */
	public void CTXCommandMenu.commandExecutable(EventObject commandEvent) {
//		checkEnabled();
	}

	/**
	 * Description of the Method
	 *
	 * @param commandEvent  Description of the Parameter
	 */
	public void CTXCommandMenu.commandNotExecutable(EventObject commandEvent) {
//		checkEnabled();
	}


//	@AJHD Moved to the redirector (RL) refactoring for UndoableCommand
//	public void UndoableCommand.commandExecuted(EventObject commandEvent) {
//		getEventDispatcher().fireCommandExecutedEvent();
//	}
//
//	public void UndoableCommand.commandExecutable(EventObject commandEvent) {
//		getEventDispatcher().fireCommandExecutableEvent();
//	}
//
//	public void UndoableCommand.commandNotExecutable(EventObject commandEvent) {
//		getEventDispatcher().fireCommandNotExecutableEvent();
//	}

	
	//---- CB instances ----
	
	//init the command event dispatcher in the AbstractCommand constructor
	after(AbstractCommand command) returning: this(command) && execution(AbstractCommand.new(String, DrawingEditor, boolean)) { 
		command.setEventDispatcher(command.createEventDispatcher());
	}
	
	//notifications 
	after(AbstractCommand command, DrawingView oldView, DrawingView newView) : 
			this(command) && 
			args(oldView, newView) && 
			execution(void AbstractCommand.viewSelectionChanged(DrawingView, DrawingView)) {
		if (command.isViewRequired()) {
			boolean isOldViewInteractive = (oldView != null) && oldView.isInteractive();
			boolean isNewViewInteractive = (newView != null) && newView.isInteractive();
			// old view was not interactive aware while new view is now interactive aware
			if (!isOldViewInteractive && isNewViewInteractive) {
				command.getEventDispatcher().fireCommandExecutableEvent();
			}
			// old view was interactive aware while new view is not
			else if (isOldViewInteractive && !isNewViewInteractive) {
				command.getEventDispatcher().fireCommandNotExecutableEvent();
			}
		}

	}
	
	//registration of Command listeners/observers
	
	after(CTXCommandMenu menu, JMenuItem m) : 
		this(menu) &&
		args(m) &&
		execution(void CTXCommandMenu.addMenuItem(JMenuItem)) {
			((CommandHolder)m).getCommand().addCommandListener(menu/*this*/);
	}

	after(CommandMenu menu, Command command, JMenuItem m) : 
		this(menu) &&
		args(command, m) &&
		execution(void CommandMenu.addMenuItem(Command, JMenuItem)) {
			command.addCommandListener(menu/*this*/);
	}

//	after(UndoableCommand uc) : 
//		this(uc) &&
//		execution(UndoableCommand.new(Command)) {
//			uc.getWrappedCommand().addCommandListener(uc/*this*/);
//	}
	
	//It would have been desired to use introduction for this support class, just like for members.
	//Introduction for support (nested) classes is not supported in AspectJ (current version). 
	//This solution moves the dispatcher from AbstractComamnd to this aspect that deals with the ObservableCommand role.
	public static class EventDispatcher {
		private List myRegisteredListeners;
		private Command myObservedCommand;

		public EventDispatcher(Command newObservedCommand) {
			myRegisteredListeners = CollectionsFactory.current().createList();
			myObservedCommand = newObservedCommand;
		}

		public void fireCommandExecutedEvent() {
			Iterator iter = myRegisteredListeners.iterator();
			while (iter.hasNext()) {
				((CommandListener)iter.next()).commandExecuted(new EventObject(myObservedCommand));
			}
		}

		public void fireCommandExecutableEvent() {
			Iterator iter = myRegisteredListeners.iterator();
			while (iter.hasNext()) {
				((CommandListener)iter.next()).commandExecutable(new EventObject(myObservedCommand));
			}
		}

		public void fireCommandNotExecutableEvent() {
			Iterator iter = myRegisteredListeners.iterator();
			while (iter.hasNext()) {
				((CommandListener)iter.next()).commandNotExecutable(new EventObject(myObservedCommand));
			}
		}

		public void addCommandListener(CommandListener newCommandListener) {
			if (!myRegisteredListeners.contains(newCommandListener)) {
				myRegisteredListeners.add(newCommandListener);
			}
		}

		public void removeCommandListener(CommandListener oldCommandListener) {
			if (myRegisteredListeners.contains(oldCommandListener)) {
				myRegisteredListeners.remove(oldCommandListener);
			}
		}
	}

}

