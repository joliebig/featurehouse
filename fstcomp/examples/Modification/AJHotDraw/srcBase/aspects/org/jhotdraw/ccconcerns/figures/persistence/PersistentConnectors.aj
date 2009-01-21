package org.jhotdraw.ccconcerns.figures.persistence;

import java.io.IOException;

import org.jhotdraw.framework.Connector;
import org.jhotdraw.framework.Figure;
import org.jhotdraw.framework.Locator;
import org.jhotdraw.standard.AbstractConnector;
import org.jhotdraw.standard.LocatorConnector;
import org.jhotdraw.util.Storable;
import org.jhotdraw.util.StorableInput;
import org.jhotdraw.util.StorableOutput;

/**
 * Adds the persistence concern to the Connector class and its subclasses.
 */
public privileged aspect PersistentConnectors {
	declare parents: Connector extends Storable;

	/**
	 * Stores the connector and its owner to a StorableOutput.
	 */
	public void AbstractConnector.write(StorableOutput dw) {
		dw.writeStorable(owner());
	}

	/**
	 * Reads the connector and its owner from a StorableInput.
	 */
	public void AbstractConnector.read(StorableInput dr) throws IOException {
		fOwner = (Figure) dr.readStorable();
	}


	/**
	 * Stores the arrow tip to a StorableOutput.
	 */
	public void LocatorConnector.write(StorableOutput dw) {
		super.write(dw);
		dw.writeStorable(getLocator());
	}

	/**
	 * Reads the arrow tip from a StorableInput.
	 */
	public void LocatorConnector.read(StorableInput dr) throws IOException {
		super.read(dr);
		setLocator((Locator)dr.readStorable());
	}
}
