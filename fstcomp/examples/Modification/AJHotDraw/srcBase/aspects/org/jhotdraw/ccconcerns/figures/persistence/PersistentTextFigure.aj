package org.jhotdraw.ccconcerns.figures.persistence;

import java.awt.Font;
import java.awt.Point;
import java.awt.Rectangle;

import org.jhotdraw.figures.TextFigure;
import org.jhotdraw.framework.Figure;
import org.jhotdraw.standard.OffsetLocator;
import org.jhotdraw.util.StorableInput;
import org.jhotdraw.util.StorableOutput;

/**
 * Introduce the persistence concern into the the TextFigure.
 * The aspect is privileged as it needs access to the private
 * memebers of TextFigure.
 * 
 * @author Marius M.
 *
 */
public privileged aspect PersistentTextFigure {

	/**
	 * @see org.jhotdraw.util.Storable#write(org.jhotdraw.util.StorableOutput)
	 */
	public void TextFigure.write(StorableOutput dw) {
		super.write(dw);
		Rectangle r = displayBox();
		dw.writeInt(r.x);
		dw.writeInt(r.y);
		dw.writeString(getText());
		dw.writeString(fFont.getName());
		dw.writeInt(fFont.getStyle());
		dw.writeInt(fFont.getSize());
		dw.writeBoolean(fIsReadOnly);
		dw.writeStorable(getObservedFigure());
		dw.writeStorable(getLocator());
	}

	/**
	 * @see org.jhotdraw.util.Storable#read(org.jhotdraw.util.StorableInput)
	 */
	public void TextFigure.read(StorableInput dr) /*@AJHD refactored throws IOException*/ {
		super.read(dr);
		markDirty();
		basicDisplayBox(new Point(dr.readInt(), dr.readInt()), null);
		setText(dr.readString());
		fFont = new Font(dr.readString(), dr.readInt(), dr.readInt());
		fIsReadOnly = dr.readBoolean();

		setObservedFigure((Figure) dr.readStorable());
		if (getObservedFigure() != null) {
			getObservedFigure().addFigureChangeListener(this);
		}
		setLocator((OffsetLocator) dr.readStorable());
	}

    
}
