package org.jhotdraw.ccconcerns.figures.persistence;

import java.io.IOException;

import org.jhotdraw.framework.Locator;
import org.jhotdraw.standard.AbstractLocator;
import org.jhotdraw.standard.OffsetLocator;
import org.jhotdraw.standard.RelativeLocator;
import org.jhotdraw.util.Storable;
import org.jhotdraw.util.StorableInput;
import org.jhotdraw.util.StorableOutput;

public privileged aspect PersistentLocators {
	declare parents: Locator extends Storable;

	declare parents: AbstractLocator implements Storable;

	/**
	 * Stores the arrow tip to a StorableOutput.
	 */
	public void AbstractLocator.write(StorableOutput dw) {
	}

	/**
	 * Reads the arrow tip from a StorableInput.
	 */
	public void AbstractLocator.read(StorableInput dr) throws IOException {
	}

	public void OffsetLocator.write(StorableOutput dw) {
		super.write(dw);
		dw.writeInt(fOffsetX);
		dw.writeInt(fOffsetY);
		dw.writeStorable(fBase);
	}

	public void OffsetLocator.read(StorableInput dr) throws IOException {
		super.read(dr);
		fOffsetX = dr.readInt();
		fOffsetY = dr.readInt();
		fBase = (Locator) dr.readStorable();
	}

	public void RelativeLocator.write(StorableOutput dw) {
		super.write(dw);
		dw.writeDouble(fRelativeX);
		dw.writeDouble(fRelativeY);
	}

	public void RelativeLocator.read(StorableInput dr) throws IOException {
		super.read(dr);
		fRelativeX = dr.readDouble();
		fRelativeY = dr.readDouble();
	}
}

