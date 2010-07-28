public class Test {
	public static void main(String[] args) {
		Stack<String> ss = new Stack<String>();
		ss.push("A");
		ss.push("B");
		ss.push("C");
		ss.pop();
		System.out.println(ss.top() + " " + ss.size());
	}
}