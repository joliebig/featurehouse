package org.jhotdraw.ccconcerns.figures.persistence;

import org.jhotdraw.framework.Figure;
import org.jhotdraw.samples.javadraw.AnimationDecorator;
import org.jhotdraw.standard.DecoratorFigure;
import org.jhotdraw.util.StorableInput;
import org.jhotdraw.util.StorableOutput;

/** 
 * Adds the persistence concern to the DecoratorFigure class and its subclasses.
 * @author Gijs Peek
 */
public privileged aspect PersistentDecoratorFigure {
	/**
	 * Writes itself and the contained figure to the StorableOutput.
	 */
	public void DecoratorFigure.write(StorableOutput dw) {
		super.write(dw);
		dw.writeStorable(getDecoratedFigure());
	}

	/**
	 * Reads itself and the contained figure from the StorableInput.
	 */
	public void DecoratorFigure.read(StorableInput dr) /*@AJHD refactored throws IOException*/ {
		super.read(dr);
		decorate((Figure)dr.readStorable());
	}
	
	public void AnimationDecorator.write(StorableOutput dw) {
		super.write(dw);
		dw.writeInt(fXVelocity);
		dw.writeInt(fYVelocity);
	}

	public void AnimationDecorator.read(StorableInput dr) /*@AJHD refactored throws IOException*/ {
		super.read(dr);
		fXVelocity = dr.readInt();
		fYVelocity = dr.readInt();
	}
}

