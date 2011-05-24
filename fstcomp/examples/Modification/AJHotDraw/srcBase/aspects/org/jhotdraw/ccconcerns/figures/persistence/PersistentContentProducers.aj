package org.jhotdraw.ccconcerns.figures.persistence;

import java.awt.Color;
import java.io.IOException;
import java.net.URL;
import java.util.Iterator;
import java.util.Map;

import org.jhotdraw.contrib.html.AbstractContentProducer;
import org.jhotdraw.contrib.html.AttributeFigureContentProducer;
import org.jhotdraw.contrib.html.ColorContentProducer;
import org.jhotdraw.contrib.html.ContentProducer;
import org.jhotdraw.contrib.html.ContentProducerRegistry;
import org.jhotdraw.contrib.html.FigureDataContentProducer;
import org.jhotdraw.contrib.html.ResourceContentProducer;
import org.jhotdraw.contrib.html.TextHolderContentProducer;
import org.jhotdraw.contrib.html.URLContentProducer;
import org.jhotdraw.standard.TextHolder;
import org.jhotdraw.util.Storable;
import org.jhotdraw.util.StorableInput;
import org.jhotdraw.util.StorableOutput;

public privileged aspect PersistentContentProducers {
	declare parents: ContentProducerRegistry || ContentProducer extends Storable;

	/**
	 * Storable write support
	 * 
	 * @param dw
	 *            the storable output
	 */
	public void ContentProducerRegistry.write(StorableOutput dw) {
		dw.writeInt(fContentProducers.size());
		Map.Entry producerEntry;
		Iterator iter = fContentProducers.entrySet().iterator();
		while (iter.hasNext()) {
			producerEntry = (Map.Entry) iter.next();
			dw.writeString(((Class) producerEntry.getKey()).getName());
			dw.writeStorable((Storable) producerEntry.getKey());
		}
	}

	/**
	 * Storable inoput support
	 * 
	 * @param dr
	 *            storable input
	 * @exception IOException
	 *                thrown by called methods
	 */
	public void ContentProducerRegistry.read(StorableInput dr) throws IOException {
		// read the default content producers, count first
		int prodCount = dr.readInt();
		String prodClass;
		ContentProducer producer;
		for (int cnt = 0; cnt < prodCount; cnt++) {
			prodClass = dr.readString();
			producer = (ContentProducer) dr.readStorable();
			try {
				registerContentProducer(Class.forName(prodClass), producer);
			} catch (ClassNotFoundException ex) {
				// the class does not exist in this application
				// cannot do much about it so ignore it, the entities of
				// this class will get their toString() value instead
			}
		}

	}

	/** @{inheritDoc} */
	public void AbstractContentProducer.write(StorableOutput dw) {
	}

	/** @{inheritDoc} */
	public void AbstractContentProducer.read(StorableInput dr)
			throws IOException {
	}

	/** @{inheritDoc} */
	public void TextHolderContentProducer.write(StorableOutput dw) {
		super.write(dw);
		dw.writeStorable(getTextHolder().getRepresentingFigure());
	}

	/** @{inheritDoc} */
	public void TextHolderContentProducer.read(StorableInput dr)
			throws IOException {
		super.read(dr);
		setTextHolder((TextHolder) dr.readStorable());
	}

	/** @{inheritDoc} */
	public void FigureDataContentProducer.write(StorableOutput dw) {
		super.write(dw);
	}

	/** @{inheritDoc} */
	public void FigureDataContentProducer.read(StorableInput dr)
			throws IOException {
		super.read(dr);
	}

	/** @{inheritDoc} */
	public void ResourceContentProducer.write(StorableOutput dw) {
		super.write(dw);
		dw.writeString(getResourceName());
	}

	/** @{inheritDoc} */
	public void ResourceContentProducer.read(StorableInput dr)
			throws IOException {
		super.read(dr);
		setResourceName(dr.readString());
	}

	/** @{inheritDoc} */
	public void AttributeFigureContentProducer.write(StorableOutput dw) {
		super.write(dw);
	}

	/** @{inheritDoc} */
	public void AttributeFigureContentProducer.read(StorableInput dr)
			throws IOException {
		super.read(dr);
	}

	/** @{inheritDoc} */
	public void ColorContentProducer.write(StorableOutput dw) {
		super.write(dw);
		dw.writeBoolean((getColor() != null));
		if (getColor() != null) {
			dw.writeInt(getColor().getRGB());
		}
	}

	/** @{inheritDoc} */
	public void ColorContentProducer.read(StorableInput dr) throws IOException {
		super.read(dr);
		boolean hasColor = dr.readBoolean();
		if (hasColor) {
			setColor(new Color(dr.readInt()));
		} else {
			setColor(null);
		}
	}

	/** @{inheritDoc} */
	public void URLContentProducer.write(StorableOutput dw) {
		super.write(dw);
		dw.writeBoolean((getURL() != null));
		if (getURL() != null) {
			dw.writeString(getURL().toExternalForm());
		}
	}

	/** @{inheritDoc} */
	public void URLContentProducer.read(StorableInput dr) throws IOException {
		super.read(dr);
		boolean hasURL = dr.readBoolean();
		if (hasURL) {
			setURL(new URL(dr.readString()));
		}
	}
}