
import java.util.ArrayList;
import java.util.List;

import pkjab.de.uni_passau.fim.pkjab.model.tags.AbstractXMLTag;

public class Stack {

	private final List stack = new ArrayList(); 

	private static final long serialVersionUID = 6337621223205071083L;

	public AbstractXMLTag pop() {
		return stack.isEmpty() ? null : (AbstractXMLTag) stack.remove(this.size()-1);
	}
	
	public AbstractXMLTag peek() {
		return stack.isEmpty() ? null : (AbstractXMLTag) stack.get(this.size()-1);
	}
	
	public void replaceTop(AbstractXMLTag tag) {
		pop();
		stack.add(tag);
	}
	
	public void add(AbstractXMLTag tag) {
		stack.add(tag);
	}
	
	public int size() {
		return stack.size();
	}
	
	public String toString() {
		return stack.toString();
	}	
	
	public void clear() {
		stack.clear();
	}
	
	public AbstractXMLTag get(int index) {
		return (AbstractXMLTag) stack.get(index);
	}
}
