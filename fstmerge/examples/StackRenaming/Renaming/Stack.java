public class RenamedStack<T> {
	private LinkedList<T> items = new LinkedList<T>();
	public void push(T item) {
		items.addFirst(item);
	}
}
