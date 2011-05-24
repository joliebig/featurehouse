package de.uni_passau.fim.pkjab.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import de.uni_passau.fim.pkjab.model.messages.Message;

/**
 * This class represents an observable object, or "data" in the model-view 
 * paradigm.
 * It can be subclassed to represent an object that the application wants 
 * to have observed.
 * An observable object can have zero, one or more observers. An observer may be 
 * any object that implements interface {@link Observer} and is an 
 * instance of the Generic O. After an observable instance changes, it should
 * call {@link #notifyObservers()} to let all of its observers be notified of the change by a 
 * call to their {@link Observer#update()} method.
 * The order in which notifications will be delivered is unspecified. All notifications are
 * done in the callee's thread and are blocking.
 * Note that this notification mechanism is completely separate from the wait and notify 
 * mechanism of class <code>Object</code>.  
 * When an observable object is newly created, 
 * its set of observers is empty.  Two observers are considered the same if 
 * and only if the equals method returns true for them. 
 * 
 * This class is a variation of the java.util.Observable. 
 * The java.util.Observable has not been used because it does not support 
 * type safety with generics. 
 * Other parts of the java.util.Observable have been omitted because they 
 * are not necessary in this context
 * 
 * A concrete implementation <code>ConcreteObservable</code> of this abstract class should not inherit <code>Observable</code>
 * but instead {@code Observerable<ConcreteObserver>} with a marker interface {@code ConcreteObserver extends Observer}.
 * This increases type safety.  
 *
 * @param <O> specifies which classes can observe this
 * @author Alex
 * @see Observer
 * 
 * @uml.annotations  for <code>observers</code> 
 *   collection_type="charybdis.utilities.Observable.O"
 */ 
public abstract class Observable {
	/**
	 * A list of <code>Observers</code> that will be notified of by 
	 * the <code>notifyObservers</code> method.
	 */
	private final List observers = new ArrayList();
	
	/**
	 * Adds an observer to the set of observers for this object, 
	 * provided that it is not the same as some observer already in the set.
	 * An observer which is already attached is ignored.
	 * 
	 * @param observer an observer to be added.
	 * @throws IllegalArgumentException if <code>observer</code> is <code>null</code>
	 */
	public synchronized void attach(Observer observer) {
		if (observer == null) {
			throw new NullPointerException();
		}
		if (!observers.contains(observer)) {
		    observers.add(observer);
		}
	}

	/**
	 * Removes an observer from the set of observers of this object. 
	 * Passing null or a object not in the list to this method will have no effect.
	 * @param observer the observer to be removed.
	 * @throws IllegalArgumentException if <code>observer</code> is <code>null</code>
	 */
	public synchronized void detach(Observer observer) throws IllegalArgumentException {
		observers.remove(observer);
	}

	/**
	 * Notifies all of the observers of this object. 
	 * Each observer's update method will be called.
	 */
	protected synchronized void notifyObservers(Message msg) {
		for (Iterator it = observers.iterator(); it.hasNext(); ) {
			((Observer)it.next()).update(msg);
		}
	}
	
	/**
	 * Returns an umodifiable list of all observers.
	 * All access on this list should be synchronized.
	 * 
	 * @return list of observers
	 */
	protected List getObservers() {
		return Collections.unmodifiableList(observers);
	}
}