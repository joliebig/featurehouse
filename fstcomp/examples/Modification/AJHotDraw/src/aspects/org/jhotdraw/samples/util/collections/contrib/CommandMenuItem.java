
package org.jhotdraw.contrib; 
import org.jhotdraw.util.Command; 
import javax.swing.JMenuItem; 
import javax.swing.Icon; 
import java.awt.event.ActionListener; 
import java.awt.event.ActionEvent; 
public  class  CommandMenuItem  extends JMenuItem  implements CommandHolder, ActionListener {
		private Command fCommand;

		public CommandMenuItem(Command command) {	super(command.name());	setCommand(command);	addActionListener(this);	}

		public CommandMenuItem(Command command, Icon icon) {	super(command.name(), icon);	setCommand(command);	addActionListener(this);	}

		public CommandMenuItem(Command command, int mnemonic) {	super(command.name(), mnemonic);	setCommand(command);	}

		public Command getCommand() {	return fCommand;	}

		public void setCommand(Command newCommand) {	fCommand = newCommand;	}

		public void actionPerformed(ActionEvent e) {	getCommand().execute();	}


}
