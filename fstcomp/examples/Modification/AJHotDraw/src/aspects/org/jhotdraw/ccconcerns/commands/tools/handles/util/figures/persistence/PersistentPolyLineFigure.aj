package org.jhotdraw.ccconcerns.figures.persistence;

import java.awt.Point;
import java.util.Iterator;

import org.jhotdraw.figures.LineConnection;
import org.jhotdraw.figures.LineDecoration;
import org.jhotdraw.figures.PolyLineFigure;
import org.jhotdraw.framework.Connector;
import org.jhotdraw.util.CollectionsFactory;
import org.jhotdraw.util.StorableInput;
import org.jhotdraw.util.StorableOutput;

public privileged aspect PersistentPolyLineFigure {
	public void PolyLineFigure.write(StorableOutput dw) {
		super.write(dw);
		dw.writeInt(fPoints.size());
		Iterator iter = points();
		while (iter.hasNext()) {
			Point p = (Point)iter.next();
			dw.writeInt(p.x);
			dw.writeInt(p.y);
		}
		dw.writeStorable(fStartDecoration);
		dw.writeStorable(fEndDecoration);
		dw.writeColor(fFrameColor);
	}

	public void PolyLineFigure.read(StorableInput dr) /*@AJHD throws IOException*/ {
		super.read(dr);
		int size = dr.readInt();
		fPoints = CollectionsFactory.current().createList(size);
		for (int i=0; i<size; i++) {
			int x = dr.readInt();
			int y = dr.readInt();
			fPoints.add(new Point(x,y));
		}
		setStartDecoration((LineDecoration)dr.readStorable());
		setEndDecoration((LineDecoration)dr.readStorable());
		fFrameColor = dr.readColor();
	}

	public void LineConnection.write(StorableOutput dw) {
		super.write(dw);
		dw.writeStorable(getStartConnector());
		dw.writeStorable(getEndConnector());
	}

	public void LineConnection.read(StorableInput dr) /*@AJHD throws IOException*/ {
		super.read(dr);
		Connector start = (Connector)dr.readStorable();
		if (start != null) {
			connectStart(start);
		}
		Connector end = (Connector)dr.readStorable();
		if (end != null) {
			connectEnd(end);
		}
		if ((start != null) && (end != null)) {
			updateConnection();
		}
	}
}

