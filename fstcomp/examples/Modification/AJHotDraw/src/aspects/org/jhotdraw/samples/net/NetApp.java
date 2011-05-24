
package org.jhotdraw.samples.net; 
import javax.swing.JToolBar; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.figures.*; 
import org.jhotdraw.application.DrawApplication; 
public  class  NetApp  extends DrawApplication {
		public NetApp() {	super("Net");	}

		protected void createTools(JToolBar palette) {	super.createTools(palette);	Tool tool = new TextTool(this, new NodeFigure());	palette.add(createToolButton(IMAGES + "TEXT", "Text Tool", tool));	tool = new CreationTool(this, new NodeFigure());	palette.add(createToolButton(IMAGES + "RECT", "Create Org Unit", tool));	tool = new ConnectionTool(this, new LineConnection());	palette.add(createToolButton(IMAGES + "CONN", "Connection Tool", tool));	}

		public static void main(String[] args) {	DrawApplication window = new NetApp();	window.open();	}


}
