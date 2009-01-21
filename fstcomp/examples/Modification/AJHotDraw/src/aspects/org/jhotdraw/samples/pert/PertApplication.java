
package org.jhotdraw.samples.pert; 
import javax.swing.JToolBar; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.figures.*; 
import org.jhotdraw.application.*; 
public  class  PertApplication  extends DrawApplication {
		static private final String PERTIMAGES = "/CH/ifa/draw/samples/pert/images/";

		public PertApplication() {	super("PERT Editor");	}

		protected void createTools(JToolBar palette) {	super.createTools(palette);	Tool tool = new TextTool(this, new TextFigure());	palette.add(createToolButton(IMAGES + "TEXT", "Text Tool", tool));	tool = new PertFigureCreationTool(this);	palette.add(createToolButton(PERTIMAGES + "PERT", "Task Tool", tool));	tool = new ConnectionTool(this, new PertDependency());	palette.add(createToolButton(IMAGES + "CONN", "Dependency Tool", tool));	tool = new CreationTool(this, new LineFigure());	palette.add(createToolButton(IMAGES + "Line", "Line Tool", tool));	}

		public static void main(String[] args) {	PertApplication pert = new PertApplication();	pert.open();	}


}
