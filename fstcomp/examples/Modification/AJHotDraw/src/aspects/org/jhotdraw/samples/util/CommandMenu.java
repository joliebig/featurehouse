
package org.jhotdraw.util; 
import org.jhotdraw.framework.JHotDrawRuntimeException; 
import javax.swing.*; 
import java.awt.*; 
import java.awt.event.ActionListener; 
import java.awt.event.ActionEvent; 
import java.util.*; 
public  class  CommandMenu  extends JMenu  implements ActionListener {
		private HashMap hm;

		public CommandMenu(String name) {	super(name);	hm = new HashMap();	}

		public synchronized void add(Command command) {	addMenuItem(command, new JMenuItem(command.name()));	}

		public synchronized void add(Command command, MenuShortcut shortcut) {	addMenuItem(command, new JMenuItem(command.name(), shortcut.getKey()));	}

		public synchronized void addCheckItem(Command command) {	addMenuItem(command, new JCheckBoxMenuItem(command.name()));	}

		protected void addMenuItem(Command command, JMenuItem m) {	m.setName(command.name());	m.addActionListener(this);	add(m);	hm.put(m, command);	}

		public synchronized void remove(Command command) {	throw new JHotDrawRuntimeException("not implemented");	}

		public synchronized void remove(MenuItem item) {	throw new JHotDrawRuntimeException("not implemented");	}

		public synchronized void enable(String name, boolean state) {	for (int i = 0; i < getItemCount(); i++) {	JMenuItem item = getItem(i);	if (name.equals(item.getText())) {	item.setEnabled(state);	return;	}	}	}

		public synchronized void checkEnabled() {	for (int i = 0; i < getMenuComponentCount(); i++) {	Component c = getMenuComponent(i);	Command cmd = (Command) hm.get(c);	if (cmd != null) {	c.setEnabled(cmd.isExecutable());	}	}	}

		public void actionPerformed(ActionEvent e) {	Object source = e.getSource();	for (int i = 0; i < getItemCount(); i++) {	JMenuItem item = getItem(i);	if (source == item) {	Command cmd = (Command) hm.get(item);	if (cmd != null) { cmd.execute();	}	break;	}	}	}


}
