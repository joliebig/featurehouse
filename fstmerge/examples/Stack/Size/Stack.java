import java.util.LinkedList;

public class Stack<T> {
	private LinkedList<T> items = new LinkedList<T>();
	public Stack() {
		int j = 0;
		int k = 0;
	}
	public void push(T item) {
		items.addFirst(item);
	}
	public int size() {
		return items.size();
	}
	public T pop() {
		int i = 0;
		if(items.size() > 0)
			return items.removeFirst();
		else
			return null;
	}
}