
package org.jhotdraw.contrib; 
import java.awt.event.*; 
import java.beans.*; 
import javax.swing.*; 
import javax.swing.event.*; 
import org.jhotdraw.contrib.CTXCommandMenu; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.standard.AbstractCommand; 
public  class  CTXWindowMenu  extends CTXCommandMenu {
		MDIDesktopPane desktop;

		private CommandMenuItem cascadeCommand;

		private CommandMenuItem tileHCommand;

		private CommandMenuItem tileVCommand;

		private CommandMenuItem arrangeHCommand;

		private CommandMenuItem arrangeVCommand;

		private int staticItems;

		public CTXWindowMenu(String newText, MDIDesktopPane newDesktop, DrawingEditor newEditor) {	super(newText);	this.desktop = newDesktop;	cascadeCommand = new CommandMenuItem(	new AbstractCommand("Cascade", newEditor) {	public void execute() {	CTXWindowMenu.this.desktop.cascadeFrames();	}	public boolean isExecutable() {	return super.isExecutable() && (CTXWindowMenu.this.desktop.getAllFrames().length > 0);	}	});	tileHCommand = new CommandMenuItem(	new AbstractCommand("Tile Horizontally", newEditor) {	public void execute() {	CTXWindowMenu.this.desktop.tileFramesHorizontally();	}	public boolean isExecutable() {	return super.isExecutable() && (CTXWindowMenu.this.desktop.getAllFrames().length > 0);	}	});	tileVCommand = new CommandMenuItem(	new AbstractCommand("Tile Vertically", newEditor) {	public void execute() {	CTXWindowMenu.this.desktop.tileFramesVertically();	}	public boolean isExecutable() {	return super.isExecutable() && (CTXWindowMenu.this.desktop.getAllFrames().length > 0);	}	});	arrangeHCommand = new CommandMenuItem(	new AbstractCommand("Arrange Horizontally", newEditor) {	public void execute() {	CTXWindowMenu.this.desktop.arrangeFramesHorizontally();	}	public boolean isExecutable() {	return super.isExecutable() && (CTXWindowMenu.this.desktop.getAllFrames().length > 0);	}	});	arrangeVCommand = new CommandMenuItem(	new AbstractCommand("Arrange Vertically", newEditor) {	public void execute() {	CTXWindowMenu.this.desktop.arrangeFramesVertically();	}	public boolean isExecutable() {	return super.isExecutable() && (CTXWindowMenu.this.desktop.getAllFrames().length > 0);	}	});	addMenuListener(	new MenuListener() {	public void menuCanceled(MenuEvent e) { }	public void menuDeselected(MenuEvent e) {	removeWindowsList();	}	public void menuSelected(MenuEvent e) {	buildChildMenus();	}	});	add(cascadeCommand);	add(tileHCommand);	add(tileVCommand);	add(arrangeHCommand);	add(arrangeVCommand);	staticItems = 5;	}

		protected void removeWindowsList() {	while (this.getItemCount() > staticItems) {	remove(staticItems);	}	}

		void buildChildMenus() {	JInternalFrame[] array = desktop.getAllFrames();	cascadeCommand.setEnabled(array.length > 0);	tileHCommand.setEnabled(array.length > 0);	tileVCommand.setEnabled(array.length > 0);	arrangeHCommand.setEnabled(array.length > 0);	arrangeVCommand.setEnabled(array.length > 0);	if (array.length == 0) {	return;	}	addSeparator();	for (int i = 0; i < array.length; i++) {	ChildMenuItem menu = new ChildMenuItem(array[i]);	menu.setState(i == 0);	menu.addActionListener(	new ActionListener() {	public void actionPerformed(ActionEvent ae) {	JInternalFrame frame = ((ChildMenuItem)ae.getSource()).getFrame();	frame.moveToFront();	try {	frame.setSelected(true);	}	catch (PropertyVetoException e) {	e.printStackTrace();	}	}	});	menu.setIcon(array[i].getFrameIcon());	add(menu);	}	}

		 	class  ChildMenuItem  extends JCheckBoxMenuItem {
			private JInternalFrame frame;

			public ChildMenuItem(JInternalFrame newFrame) {	super(newFrame.getTitle());	frame = newFrame;	}

			public JInternalFrame getFrame() {	return frame;	}


	}


}
