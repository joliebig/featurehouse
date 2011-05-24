import java.util.LinkedList;

public class Stack<T> {
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
}