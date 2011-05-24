
package org.jhotdraw.figures; 
import org.jhotdraw.framework.Figure; 
public  class  NumberTextFigure  extends TextFigure {
		private static final long serialVersionUID = -4056859232918336475L;

		private int numberTextFigureSerializedDataVersion = 1;

		public int overlayColumns() {	return Math.max(4, getText().length());	}

		public int getValue() {	int value = 0;	try {	value = Integer.parseInt(getText());	}	catch (NumberFormatException e) {	value = 0;	}	return value;	}

		public void setValue(int value) {	setText(Integer.toString(value));	}

		public Figure getRepresentingFigure() {	return this;	}


}
