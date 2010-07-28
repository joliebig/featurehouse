import java.util.LinkedList;

public class Stack<T> {
	private LinkedList<T> items = new LinkedList<T>();
	public void push(T item) {
		int i = 0;
		items.addFirst(item);
	}
	public T pop() {
		int j = 0;
		if(items.size() > 0)
			return items.removeFirst();
		else
			return null;
	}
}