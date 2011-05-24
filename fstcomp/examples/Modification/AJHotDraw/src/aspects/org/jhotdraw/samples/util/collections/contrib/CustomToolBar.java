
package org.jhotdraw.contrib; 
import org.jhotdraw.util.CollectionsFactory; 
import javax.swing.JToolBar; 
import javax.swing.JComponent; 
import java.util.List; 
import java.util.Iterator; 
import java.awt.Component; 
public  class  CustomToolBar  extends JToolBar {
		private List standardTools;

		private List editTools;

		private List currentTools;

		private boolean needsUpdate;

		public CustomToolBar() {	super();	standardTools = CollectionsFactory.current().createList();	editTools = CollectionsFactory.current().createList();	currentTools = standardTools;	needsUpdate = false;	}

		public void switchToolBar() {	if (currentTools == standardTools) {	switchToEditTools();	}	else {	switchToStandardTools();	}	}

		public void switchToEditTools() {	if (currentTools != editTools) {	currentTools = editTools;	needsUpdate = true;	}	}

		public void switchToStandardTools() {	if (currentTools != standardTools) {	currentTools = standardTools;	needsUpdate = true;	}	}

		public void activateTools() {	if (!needsUpdate) {	return;	}	else {	removeAll();	JComponent currentTool = null;	Iterator iter = currentTools.iterator();	while (iter.hasNext()) {	currentTool = (JComponent)iter.next();	super.add(currentTool);	}	validate();	needsUpdate = false;	}	}

		public Component add(Component newTool) {	if (currentTools == editTools) {	editTools.add(newTool);	}	else {	standardTools.add(newTool);	}	needsUpdate = true;	return super.add(newTool);	}


}
