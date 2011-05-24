package org.jhotdraw.ccconcerns.figures.persistence;

import java.awt.Rectangle;
import java.util.Iterator;
import java.util.List;

import org.jhotdraw.contrib.GraphicalCompositeFigure;
import org.jhotdraw.contrib.Layouter;
import org.jhotdraw.framework.Figure;
import org.jhotdraw.framework.FigureEnumeration;
import org.jhotdraw.samples.pert.PertFigure;
import org.jhotdraw.standard.CompositeFigure;
import org.jhotdraw.util.CollectionsFactory;
import org.jhotdraw.util.Storable;
import org.jhotdraw.util.StorableInput;
import org.jhotdraw.util.StorableOutput;

/**
 * Implements the "persistence" concern for composite figures.
 * The aspect accesses fields of the class into which introduces
 * the persistence concern (e.g., fFigures - protected list of the 
 * component figures of a composite), therefore needs to be given 
 * privileged access.
 * 
 * @author Marius M.
 * @author Gijs Peek
 *
 */
public privileged aspect PersistentCompositeFigure {
	
	public void CompositeFigure.write(StorableOutput dw) {
		super.write(dw);
		int cnt = figureCount();
		dw.writeInt(cnt);
		FigureEnumeration fe = figures();
		int i = 0;
		while (fe.hasNextFigure()) {
			i++;
			Figure f = fe.nextFigure();
			assert f != null;
			dw.writeStorable(f);
		}
		assert cnt == i;
	}


	public void CompositeFigure.read(StorableInput dr) /*@AJHD refactored throws IOException*/ { 
		super.read(dr);
		int size = dr.readInt();
		fFigures = CollectionsFactory.current().createList(size);
	
		for (int i=0; i<size; i++) {
			Storable s = dr.readStorable();
			assert s != null : "reading null storable";
			add((Figure)s);
		}
		init(displayBox());
	}


	/**
	 * Reads the contained figures from StorableInput. The figure responsible
	 * for graphical presentation is read together with all child components.
	 * The Layouter is not stored and therefore not read.
	 */
	public void GraphicalCompositeFigure.read(StorableInput dr) /* AJHD refactored throws IOException */ {
		super.read(dr);
		setPresentationFigure((Figure) dr.readStorable());
		setLayouter((Layouter) dr.readStorable());
	}

	/**
	 * Writes the contained figures to the StorableOutput. The figure
	 * responsible for graphical presentation is written together with all child
	 * components. The Layouter is not written.
	 */
	public void GraphicalCompositeFigure.write(StorableOutput dw) {
		super.write(dw);
		dw.writeStorable(getPresentationFigure());
		dw.writeStorable(getLayouter());
	}
	
	public void PertFigure.write(StorableOutput dw) {
		super.write(dw);
		dw.writeInt(fDisplayBox.x);
		dw.writeInt(fDisplayBox.y);
		dw.writeInt(fDisplayBox.width);
		dw.writeInt(fDisplayBox.height);

		writeTasks(dw, fPreTasks);
		writeTasks(dw, fPostTasks);
	}

	public void PertFigure.writeTasks(StorableOutput dw, List l) {
		dw.writeInt(l.size());
		Iterator iter = l.iterator();
		while (iter.hasNext()) {
			dw.writeStorable((Storable)iter.next());
		}
	}

	public void PertFigure.read(StorableInput dr) /* AJHD refactored throws IOException */ {
		super.read(dr);
		fDisplayBox = new Rectangle(
			dr.readInt(),
			dr.readInt(),
			dr.readInt(),
			dr.readInt());
		layout();
		fPreTasks = readTasks(dr);
		fPostTasks = readTasks(dr);
	}
	
	public List PertFigure.readTasks(StorableInput dr) /* AJHD refactored throws IOException */ {
		int size = dr.readInt();
		List l = CollectionsFactory.current().createList(size);
		for (int i=0; i<size; i++) {
			l.add(dr.readStorable());
		}
		return l;
	}
}

