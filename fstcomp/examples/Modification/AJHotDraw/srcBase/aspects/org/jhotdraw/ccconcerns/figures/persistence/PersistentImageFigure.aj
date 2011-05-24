package org.jhotdraw.ccconcerns.figures.persistence;

import java.awt.Rectangle;

import org.jhotdraw.figures.ImageFigure;
import org.jhotdraw.util.Iconkit;
import org.jhotdraw.util.StorableInput;
import org.jhotdraw.util.StorableOutput;

/**
 * The persistence concern for the Image Figure.
 * The aspect is privileged as it needs access to private members
 * of ImageFigure.
 * 
 * @author Marius M.
 *
 */
public privileged aspect PersistentImageFigure {

  /**
	* Writes the ImageFigure to a StorableOutput. Only a reference to the
	* image, that is its pathname is saved.
	*/
	public void ImageFigure.write(StorableOutput dw) {
		super.write(dw);
		dw.writeInt(fDisplayBox.x);
		dw.writeInt(fDisplayBox.y);
		dw.writeInt(fDisplayBox.width);
		dw.writeInt(fDisplayBox.height);
		dw.writeString(fFileName);
	}

   /**
	* Reads the ImageFigure from a StorableInput. It registers the
	* referenced figure to be loaded from the Iconkit.
	* @see Iconkit#registerImage
	*/
	public void ImageFigure.read(StorableInput dr) /*@AJHD refactored throws IOException*/ {
		super.read(dr);
		fDisplayBox = new Rectangle(
			dr.readInt(),
			dr.readInt(),
			dr.readInt(),
			dr.readInt());
		fFileName = dr.readString();
		Iconkit.instance().registerImage(fFileName);
	}
}
