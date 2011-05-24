package org.jhotdraw.ccconcerns.figures.persistence;

import java.awt.Insets;
import java.io.IOException;

import org.jhotdraw.contrib.Layoutable;
import org.jhotdraw.contrib.Layouter;
import org.jhotdraw.contrib.SimpleLayouter;
import org.jhotdraw.contrib.html.HTMLLayouter;
import org.jhotdraw.util.Storable;
import org.jhotdraw.util.StorableInput;
import org.jhotdraw.util.StorableOutput;

public aspect PersistentLayouters {
	declare parents: Layouter extends Storable;

	/**
	 * Reads the contained figures from StorableInput.
	 */
	public void SimpleLayouter.read(StorableInput dr) throws IOException {
		setLayoutable((Layoutable)dr.readStorable());
		setInsets(new Insets(dr.readInt(), dr.readInt(), dr.readInt(), dr.readInt()));
	}
	
	/**
	 * Writes the contained figures to the StorableOutput.
	 */
	public void SimpleLayouter.write(StorableOutput dw) {
		dw.writeStorable(getLayoutable());
		Insets i = getInsets();
		dw.writeInt(i.top);
		dw.writeInt(i.left);
		dw.writeInt(i.bottom);
		dw.writeInt(i.right);
	}

	/**
	 * Description of the Method
	 *
	 * @param dw  Description of the Parameter
	 */
	public void HTMLLayouter.write(StorableOutput dw) {
		/**
		 * @todo:   Implement this org.jhotdraw.util.Storable method
		 */
		throw new UnsupportedOperationException("Method write() not yet implemented.");
	}

	/**
	 * Description of the Method
	 *
	 * @param dr               Description of the Parameter
	 * @exception IOException  Description of the Exception
	 */
	public void HTMLLayouter.read(StorableInput dr) throws IOException {
		/**
		 * @todo:   Implement this org.jhotdraw.util.Storable method
		 */
		throw new UnsupportedOperationException("Method read() not yet implemented.");
	}
}
