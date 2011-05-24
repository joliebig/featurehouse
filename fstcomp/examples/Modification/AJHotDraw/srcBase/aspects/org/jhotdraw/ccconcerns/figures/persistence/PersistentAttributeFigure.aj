package org.jhotdraw.ccconcerns.figures.persistence;

import java.awt.Polygon;
import java.awt.Rectangle;

import org.jhotdraw.contrib.PolygonFigure;
import org.jhotdraw.contrib.TextAreaFigure;
import org.jhotdraw.contrib.TriangleFigure;
import org.jhotdraw.contrib.html.HTMLTextAreaFigure;
import org.jhotdraw.figures.AttributeFigure;
import org.jhotdraw.figures.EllipseFigure;
import org.jhotdraw.figures.FigureAttributes;
import org.jhotdraw.figures.RectangleFigure;
import org.jhotdraw.figures.RoundRectangleFigure;
import org.jhotdraw.framework.Figure;
import org.jhotdraw.standard.OffsetLocator;
import org.jhotdraw.util.StorableInput;
import org.jhotdraw.util.StorableOutput;

/**
 * Adds the persistence concern to AttributeFigure.
 * 
 * The aspect is declared privileged as it needs access to the 
 * 'fAttributes' private member of the AttributeFigure class.
 * (The member does not exclusively belong to the persistence concern,
 * so it does not belong to this aspect.)
 * 
 * @author Marius M.
 */
public privileged aspect PersistentAttributeFigure {

    /**
     * Write an attribute figure to a storable output.
     */
	public void AttributeFigure.write(StorableOutput dw) {
		super.write(dw);
		if (fAttributes == null) {
			dw.writeString("no_attributes");
		}
		else {
			dw.writeString("attributes");
			fAttributes.write(dw);
		}
	}

    /**
     * Read an attribute figure from a storable input.
     */
	public void AttributeFigure.read(StorableInput dr) /*@AJHD refactored throws IOException*/ { 
		super.read(dr);
		String s = dr.readString();
		if (s.toLowerCase().equals("attributes")) {
			fAttributes = new FigureAttributes();
			fAttributes.read(dr);
		}
	}


	public void EllipseFigure.write(StorableOutput dw) {
		super.write(dw);
		dw.writeInt(fDisplayBox.x);
		dw.writeInt(fDisplayBox.y);
		dw.writeInt(fDisplayBox.width);
		dw.writeInt(fDisplayBox.height);
	}

	public void EllipseFigure.read(StorableInput dr) /*@AJHD refactored throws IOException*/ {
		super.read(dr);
		fDisplayBox = new Rectangle(
			dr.readInt(),
			dr.readInt(),
			dr.readInt(),
			dr.readInt());
	}

	public void PolygonFigure.write(StorableOutput dw) {
		super.write(dw);
		dw.writeInt(pointCount());
		for (int i = 0; i < pointCount(); ++i) {
			dw.writeInt(getInternalPolygon().xpoints[i]);
			dw.writeInt(getInternalPolygon().ypoints[i]);
		}
	}

	public void PolygonFigure.read(StorableInput dr) /*@AJHD refactored throws IOException*/ {
		super.read(dr);
		int size = dr.readInt();
		int[] xs = new int[size];
		int[] ys = new int[size];
		for (int i = 0; i < size; i++) {
			xs[i] = dr.readInt();
			ys[i] = dr.readInt();
		}
		setInternalPolygon(new Polygon(xs, ys, size));
	}

	public void RectangleFigure.write(StorableOutput dw) {
		super.write(dw);
		dw.writeInt(fDisplayBox.x);
		dw.writeInt(fDisplayBox.y);
		dw.writeInt(fDisplayBox.width);
		dw.writeInt(fDisplayBox.height);
	}

	public void RectangleFigure.read(StorableInput dr) /*@AJHD refactored throws IOException*/ {
		super.read(dr);
		fDisplayBox = new Rectangle(
			dr.readInt(),
			dr.readInt(),
			dr.readInt(),
			dr.readInt());
	}
	
	public void RoundRectangleFigure.write(StorableOutput dw) {
		super.write(dw);
		dw.writeInt(fDisplayBox.x);
		dw.writeInt(fDisplayBox.y);
		dw.writeInt(fDisplayBox.width);
		dw.writeInt(fDisplayBox.height);
		dw.writeInt(fArcWidth);
		dw.writeInt(fArcHeight);
	}

	public void RoundRectangleFigure.read(StorableInput dr) /*@AJHD refactored throws IOException*/ {
		super.read(dr);
		fDisplayBox = new Rectangle(
			dr.readInt(),
			dr.readInt(),
			dr.readInt(),
			dr.readInt());
		fArcWidth = dr.readInt();
		fArcHeight = dr.readInt();
	}

	public void TriangleFigure.write(StorableOutput dw) {
		super.write(dw);
		dw.writeInt(fRotation);
	}

	public void TriangleFigure.read(StorableInput dr) /*@AJHD refactored throws IOException*/ {
		super.read(dr);
		fRotation = dr.readInt();
	}

	/**
	 * Writes the figure to StorableOutput
	 *
	 * @param dw  the output storable
	 */
	public void TextAreaFigure.write(StorableOutput dw) {
		super.write(dw);
		dw.writeInt(fDisplayBox.x);
		dw.writeInt(fDisplayBox.y);
		dw.writeInt(fDisplayBox.width);
		dw.writeInt(fDisplayBox.height);
		dw.writeString(fText);
		dw.writeBoolean(fIsReadOnly);
		dw.writeStorable(fObservedFigure);
		dw.writeStorable(fLocator);
	}

	/**
	 * Reads the figure from StorableInput
	 *
	 * @param dr            Description of the Parameter
	 * @throws IOException  the inout storable
	 */
	public void TextAreaFigure.read(StorableInput dr) /*@AJHD refactored throws IOException*/ {
		super.read(dr);

		markSizeDirty();
		markTextDirty();
		markFontDirty();

		fDisplayBox.x = dr.readInt();
		fDisplayBox.y = dr.readInt();
		fDisplayBox.width = dr.readInt();
		fDisplayBox.height = dr.readInt();
		fText = dr.readString();
		fIsReadOnly = dr.readBoolean();

		fObservedFigure = (Figure)dr.readStorable();
		if (fObservedFigure != null) {
			fObservedFigure.addFigureChangeListener(this);
		}
		fLocator = (OffsetLocator)dr.readStorable();

		setFont(createFont());
	}

	/**
	 * Reads the figure from StorableInput
	 *
	 * @param dr            Description of the Parameter
	 * @throws IOException  the inout storable
	 */
	public void HTMLTextAreaFigure.read(StorableInput dr) /*@AJHD refactored throws IOException*/ {
		super.read(dr);

		setFrameFigure((Figure)dr.readStorable());
		setUseDirectDraw(dr.readBoolean());
		setRawHTML(dr.readBoolean());
//		setIntrinsicContentProducer((ContentProducer)dr.readStorable());
//		fContentProducers.read(dr);

//        // finally add the popup menu
//        setAttribute(Figure.POPUP_MENU, createPopupMenu());

	}

	/**
	 * Writes the figure to StorableOutput
	 *
	 * @param dw  the output storable
	 */
	public void HTMLTextAreaFigure.write(StorableOutput dw) {
		super.write(dw);

		dw.writeStorable(getFrameFigure());
		dw.writeBoolean(usesDirectDraw());
		dw.writeBoolean(isRawHTML());
//		dw.writeStorable(getIntrinsicContentProducer());
//		fContentProducers.write(dw);
	}
}
