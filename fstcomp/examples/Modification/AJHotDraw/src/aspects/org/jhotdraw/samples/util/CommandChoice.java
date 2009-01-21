
package org.jhotdraw.util; 
import javax.swing.*; 
import java.awt.event.ItemEvent; 
import java.awt.event.ItemListener; 
import java.util.List; 
public  class  CommandChoice  extends JComboBox  implements ItemListener {
		private List fCommands;

		public CommandChoice() {	super();	fCommands = CollectionsFactory.current().createList(10);	addItemListener(this);	}

		public synchronized void addItem(Command command) {	addItem(command.name());	fCommands.add(command);	}

		public void itemStateChanged(ItemEvent e) {	if ((getSelectedIndex() >= 0) && (getSelectedIndex() < fCommands.size())) {	Command command = (Command)fCommands.get(getSelectedIndex()); if (command.isExecutable()) {	command.execute();	}	}	}


}
