
package org.jhotdraw.contrib; 
import java.awt.MenuItem; 
import java.awt.MenuShortcut; 
import java.awt.event.ActionEvent; 
import java.awt.event.ActionListener; 
import javax.swing.JMenu; 
import javax.swing.JMenuItem; 
import javax.swing.JSeparator; 
import org.jhotdraw.framework.JHotDrawRuntimeException; 
import org.jhotdraw.util.Command; 
import org.jhotdraw.util.CommandMenu; 
public  class  CTXCommandMenu  extends JMenu  implements ActionListener {
		public CTXCommandMenu(String name) {	super(name);	}

		public synchronized void add(Command command) {	addMenuItem(new CommandMenuItem(command));	}

		public synchronized void add(Command command, MenuShortcut shortcut) {	addMenuItem(new CommandMenuItem(command, shortcut.getKey()));	}

		public synchronized void addCheckItem(Command command) {	addMenuItem(new CommandCheckBoxMenuItem(command));	}

		public synchronized void add(CommandMenuItem item) {	addMenuItem(item);	}

		public synchronized void add(CommandCheckBoxMenuItem checkItem) {	addMenuItem(checkItem);	}

		protected void addMenuItem(JMenuItem m) {	m.addActionListener(this);	add(m);	}

		public synchronized void remove(Command command) {	throw new JHotDrawRuntimeException("not implemented");	}

		public synchronized void remove(MenuItem item) {	throw new JHotDrawRuntimeException("not implemented");	}

		public synchronized void enable(String name, boolean state) {	for (int i = 0; i < getItemCount(); i++) {	JMenuItem item = getItem(i);	if (name.equals(item.getText())) {	item.setEnabled(state);	return;	}	}	}

		public synchronized void checkEnabled() {	int j = 0;	for (int i = 0; i < getMenuComponentCount(); i++) {	JMenuItem currentItem = getItem(i);	if (currentItem instanceof CommandMenu) {	((CommandMenu)currentItem).checkEnabled();	}	else if (currentItem instanceof CTXCommandMenu) {	((CTXCommandMenu)currentItem).checkEnabled();	}	else if (currentItem instanceof CommandHolder) {	currentItem.setEnabled(((CommandHolder)currentItem).getCommand().isExecutable());	}	else if (currentItem instanceof Command) {	currentItem.setEnabled(((Command)currentItem).isExecutable());	}	j++;	}	}

		public void actionPerformed(ActionEvent e) {	int j = 0;	Object source = e.getSource();	for (int i = 0; i < getItemCount(); i++) {	if (getMenuComponent(i) instanceof JSeparator) {	continue;	}	JMenuItem item = getItem(i);	if (source == item) {	Command cmd = ((CommandHolder)item).getCommand();	cmd.execute();	break;	}	j++;	}	}


}
