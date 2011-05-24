
package org.jhotdraw.standard; 
import java.awt.Rectangle; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.FigureEnumeration; 
public  class  AlignCommand  extends AbstractCommand {
		public static abstract  class  Alignment {
			public final static Alignment LEFTS = new Alignment("Lefts") {	public void moveBy(Figure f, Rectangle anchor) {	Rectangle rr = f.displayBox();	f.moveBy(anchor.x-rr.x, 0);	}	};

			public final static Alignment CENTERS = new Alignment("Centers") {	public void moveBy(Figure f, Rectangle anchor) {	Rectangle rr = f.displayBox();	f.moveBy((anchor.x+anchor.width/2) - (rr.x+rr.width/2), 0);	}	};

			public final static Alignment RIGHTS = new Alignment("Rights") {	public void moveBy(Figure f, Rectangle anchor) {	Rectangle rr = f.displayBox();	f.moveBy((anchor.x+anchor.width) - (rr.x+rr.width), 0);	}	};

			public final static Alignment TOPS = new Alignment("Tops") {	public void moveBy(Figure f, Rectangle anchor) {	Rectangle rr = f.displayBox();	f.moveBy(0, anchor.y-rr.y);	}	};

			public final static Alignment MIDDLES = new Alignment("Middles") {	public void moveBy(Figure f, Rectangle anchor) {	Rectangle rr = f.displayBox();	f.moveBy(0, (anchor.y+anchor.height/2) - (rr.y+rr.height/2));	}	};

			public final static Alignment BOTTOMS = new Alignment("Bottoms") {	public void moveBy(Figure f, Rectangle anchor) {	Rectangle rr = f.displayBox();	f.moveBy(0, (anchor.y+anchor.height) - (rr.y+rr.height));	}	};

			private String myDescription;

			private Alignment(String newDescription) {	setDescription(newDescription);	}

			public String toString() {	return getDescription();	}

			public String getDescription() {	return myDescription;	}

			private void setDescription(String newDescription) {	myDescription = newDescription;	}

			public abstract void moveBy(Figure f, Rectangle anchor);


	}

		private Alignment myAlignment;

		public AlignCommand(Alignment newAlignment, DrawingEditor newDrawingEditor) {	super(newAlignment.getDescription(), newDrawingEditor);	setAlignment(newAlignment);	}

		protected boolean isExecutableWithView() {	return view().selectionCount() > 1;	}

		public void execute() {	FigureEnumeration fe = view().selection();	Figure anchorFigure = fe.nextFigure();	Rectangle r = anchorFigure.displayBox();	while (fe.hasNextFigure()) {	Figure f = fe.nextFigure();	getAlignment().moveBy(f, r);	}	}

		protected void setAlignment(Alignment newAlignment) {	myAlignment = newAlignment;	}

		public Alignment getAlignment() {	return myAlignment;	}


}
