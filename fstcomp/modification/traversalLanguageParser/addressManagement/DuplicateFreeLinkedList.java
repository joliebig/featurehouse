/**
 * 
 */
package modification.traversalLanguageParser.addressManagement;

import java.util.Collection;
import java.util.LinkedList;

/**
 * @author Boxleitner Stefan
 * 
 */
public class DuplicateFreeLinkedList<E> extends LinkedList<E> {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public DuplicateFreeLinkedList() {
	super();
    }

    public boolean add(E e) {
	if (!super.contains(e)) {
	    return super.add(e);
	}
	return false;
    }

    public boolean addAll(Collection<? extends E> c) {
	boolean flag = false;
	for (E e : c) {
	    if (add(e) == true && !flag)
		flag = true;
	}
	return flag;
    }
}
