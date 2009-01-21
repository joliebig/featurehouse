
package org.jhotdraw.util; 
import javax.swing.*; 
import java.awt.event.ActionListener; 
import java.awt.event.ActionEvent; 
public  class  CommandButton 	extends JButton  implements ActionListener {
		private Command fCommand;

		public CommandButton(Command command) {	super(command.name());	fCommand = command;	addActionListener(this);	}

		public void actionPerformed(ActionEvent e) {	fCommand.execute();	if (!getText().equals(fCommand.name()) ) {	setText(fCommand.name());	}	}


}
