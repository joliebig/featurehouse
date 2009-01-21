package org.jhotdraw.ccconcerns.figures.persistence;

import java.io.IOException;

import org.jhotdraw.figures.AbstractLineDecoration;
import org.jhotdraw.figures.ArrowTip;
import org.jhotdraw.figures.FigureAttributes;
import org.jhotdraw.figures.LineDecoration;
import org.jhotdraw.framework.FigureAttributeConstant;
import org.jhotdraw.util.Storable;
import org.jhotdraw.util.StorableInput;
import org.jhotdraw.util.StorableOutput;

public privileged aspect PersistentLineDecorations {
	declare parents: LineDecoration extends Storable;

	/**
	 * Stores the arrow tip to a StorableOutput.
	 */
	public void AbstractLineDecoration.write(StorableOutput dw) {
		if (getFillColor() != null) {
			FigureAttributes.writeColor(dw, FigureAttributeConstant.FILL_COLOR
					.getName(), getFillColor());
		} else {
			dw.writeString("no" + FigureAttributeConstant.FILL_COLOR.getName());
		}

		if (getBorderColor() != null) {
			FigureAttributes.writeColor(dw, FigureAttributeConstant.FRAME_COLOR
					.getName(), getBorderColor());
		} else {
			dw
					.writeString("no"
							+ FigureAttributeConstant.FRAME_COLOR.getName());
		}
	}

	/**
	 * Reads the arrow tip from a StorableInput.
	 */
	public void AbstractLineDecoration.read(StorableInput dr) throws IOException {
		String fillColorId = dr.readString();
		// read color only if one has been written
		if (fillColorId.equals(FigureAttributeConstant.FRAME_COLOR.getName())) {
			setFillColor(FigureAttributes.readColor(dr));
		}
		String borderColorId = dr.readString();
		// read color only if one has been written
		if (borderColorId.equals("BorderColor")
				|| borderColorId.equals(FigureAttributeConstant.FRAME_COLOR
						.getName())) {
			setBorderColor(FigureAttributes.readColor(dr));
		}
	}

	/**
	 * Stores the arrow tip to a StorableOutput.
	 */
	public void ArrowTip.write(StorableOutput dw) {
		dw.writeDouble(getAngle());
		dw.writeDouble(getOuterRadius());
		dw.writeDouble(getInnerRadius());
		super.write(dw);
	}

	/**
	 * Reads the arrow tip from a StorableInput.
	 */
	public void ArrowTip.read(StorableInput dr) throws IOException {
		setAngle(dr.readDouble());
		setOuterRadius(dr.readDouble());
		setInnerRadius(dr.readDouble());
		super.read(dr);
	}
}

