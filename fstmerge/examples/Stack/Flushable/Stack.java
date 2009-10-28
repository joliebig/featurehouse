import java.util.LinkedList;
import java.io.Flushable;

public class Stack<T> implements Flushable {
	private LinkedList<T> items = new LinkedList<T>();
	public void push(T item) {
		items.addFirst(item);
	}
	public T pop() {
		if(items.size() > 0)
			return items.removeFirst();
		else
			return null;
	}
	public void flush() { }
}