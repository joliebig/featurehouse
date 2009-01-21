
package org.jhotdraw.contrib; 
import javax.swing.JCheckBoxMenuItem; 
import org.jhotdraw.util.Command; 
import javax.swing.Icon; 
public  class  CommandCheckBoxMenuItem  extends JCheckBoxMenuItem  implements CommandHolder {
		Command fCommand;

		public CommandCheckBoxMenuItem(Command command) {	super(command.name());	setCommand(command);	}

		public CommandCheckBoxMenuItem(Command command, Icon icon) {	super(command.name(), icon);	setCommand(command);	}

		public CommandCheckBoxMenuItem(Command command, boolean b) {	super(command.name(), b);	setCommand(command);	}

		public CommandCheckBoxMenuItem(Command command, Icon icon, boolean b) {	super(command.name(), icon, b);	setCommand(command);	}

		public Command getCommand() {	return fCommand;	}

		public void setCommand(Command newCommand) {	fCommand = newCommand;	}


}
