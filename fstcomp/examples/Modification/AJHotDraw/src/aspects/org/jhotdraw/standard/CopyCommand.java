
package org.jhotdraw.standard; 
import org.jhotdraw.framework.DrawingEditor; 
public  class  CopyCommand  extends FigureTransferCommand {
		public CopyCommand(String name, DrawingEditor newDrawingEditor) {	super(name, newDrawingEditor);	}

		public void execute() {	copyFigures(view().selection(), view().selectionCount());	}

		protected boolean isExecutableWithView() {	return view().selectionCount() > 0;	}


}
