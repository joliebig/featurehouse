
package org.jhotdraw.standard; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.FigureAttributeConstant; 
import org.jhotdraw.framework.FigureEnumeration; 
public  class  ChangeAttributeCommand  extends AbstractCommand {
		private FigureAttributeConstant fAttribute;

		private Object fValue;

		public ChangeAttributeCommand(String name, FigureAttributeConstant attribute, Object value, DrawingEditor newDrawingEditor) {	super(name, newDrawingEditor);	fAttribute = attribute;	fValue = value;	}

		public void execute() {	FigureEnumeration fe = view().selection();	while (fe.hasNextFigure()) {	fe.nextFigure().setAttribute(fAttribute, fValue);	}	}

		public boolean isExecutableWithView() {	return view().selectionCount() > 0;	}


}
