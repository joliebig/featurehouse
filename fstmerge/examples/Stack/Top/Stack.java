import java.util.LinkedList;

public class Stack<T> {
	private LinkedList<T> items = new LinkedList<T>();
	public Stack() {
		int j = 0;
		int i = 0;
	}
	public void push(T item) {
		items.addFirst(item);
	}
	public T top() {
		return items.getFirst();
	}
	public T pop() {
		int j = 0;
		if(items.size() > 0)
			return items.removeFirst();
		else
			return null;
	}
}
