package org.jhotdraw.ccconcerns.figures.persistence;

import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;

/*
 * This aspect forces implementations of the Serializable class to appear outside the
 * org.jhotdraw.persistence package, as far as this is possible using aspects.
 */
public privileged aspect SerializableImplementation {
	pointcut serializableMethod():
		within(Serializable+)
		&& (execution(private void writeObject(ObjectOutputStream))
				|| execution(private void readObject(ObjectInputStream)));

	declare warning: serializableMethod()
	&& within(org.jhotdraw.persistence.*):
		"Serializable methods should not be implemented within the persistence package";

	/*
	 * FIXME: Serializable cannot be properly unplugged until aspectj enables
	 * inter-type declarations of final static fields, these are currently not
	 * treated as final (see aspectj bug #52105 at
	 * https://bugs.eclipse.org/bugs/show_bug.cgi?id=52105). When this bug is
	 * resolved, serializable classes can be unplugged to an aspect by: 
	 * 1. applying a declare parents for the Externalizable class 
	 * 2. moving the private readObject() and writeObject() methods to public readExternal()
	 *    and writeExternal() inter-type methods in the aspect (if these are
	 *    missing the defaultReadObject() and defaultWriteObject() method can be called)
	 * 3. moving the static final int serialVersionUID to a public
	 *    static final int <TypePattern>.serialVersionUID 
	 * Note that if no serialVersionUID was specified, the automatically generated
	 * serialVersionUID will change after this refactoring. This can be
	 * circumvented by explicitly adding a serialVersionUID that is equal to the
	 * old automatically generated one, so objects that were serialized prior to
	 * the refactoring can still be deserialized after.
	 */
}
