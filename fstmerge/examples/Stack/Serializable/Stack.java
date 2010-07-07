import java.util.LinkedList;
import java.io.Serializable;

public class Stack<T> implements Serializable {
	private static final long serialVersionUID = 42;
	public LinkedList<T> items = new LinkedList<T>();
	public void push(T item) {
		items.addFirst(item);
	}
	public T pop() {
		if(items.size() > 0)
			return items.removeFirst();
		else
			return null;
	}
}