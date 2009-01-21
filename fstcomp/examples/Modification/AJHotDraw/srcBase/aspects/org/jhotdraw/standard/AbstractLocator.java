/*
 * @(#)AbstractLocator.java
 *
 * Project:		JHotdraw - a GUI framework for technical drawings
 *				http://www.jhotdraw.org
 *				http://jhotdraw.sourceforge.net
 * Copyright:	� by the original author(s) and all contributors
 * License:		Lesser GNU Public License (LGPL)
 *				http://www.opensource.org/licenses/lgpl-license.html
 */

package org.jhotdraw.standard;

import org.jhotdraw.framework.Handle;
import org.jhotdraw.framework.Locator;


/**
 * AbstractLocator provides default implementations for
 * the Locator interface.
 *
 * @see Locator
 * @see Handle
 *
 * @version <$CURRENT_VERSION$>
 */
/*
 * AJHD: refactored persistence - the interface extends Storable
 * through introductions/declare parents from the PersistentLocators aspect. 
 */
public abstract class AbstractLocator implements Locator/*, Storable*/, Cloneable {

	/*
	 * Serialization support.
	 */
	private static final long serialVersionUID = -7742023180844048409L;

	protected AbstractLocator() {
	}

	public Object clone() {
		try {
			return super.clone();
		}
		catch (CloneNotSupportedException e) {
			throw new InternalError();
		}
	}

//	AJHD: refactored persistence - the persistence-specific methods,
//	write/read, are introduced via inter-type declarations from the
//	persistence.PersistentLocators aspect.
//	/**
//	 * Stores the arrow tip to a StorableOutput.
//	 */
//	public void write(StorableOutput dw) {
//	}
//
//	/**
//	 * Reads the arrow tip from a StorableInput.
//	 */
//	public void read(StorableInput dr) throws IOException {
//	}
}


