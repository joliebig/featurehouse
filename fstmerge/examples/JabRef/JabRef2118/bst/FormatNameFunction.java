package net.sf.jabref.bst; 

import java.util.Stack; 

import net.sf.jabref.AuthorList; 
import net.sf.jabref.AuthorList.Author; 
import net.sf.jabref.bst.VM.BstEntry; 
import net.sf.jabref.bst.VM.BstFunction; 



public  class  FormatNameFunction implements  BstFunction {
	
	
	VM vm;

	

	public FormatNameFunction(VM vm) {
		this.vm = vm;
	}


	
	
	public void execute(BstEntry context) {
		Stack<Object> stack = vm.getStack();

		if (stack.size() < 3) {
			throw new VMException("Not enough operands on stack for operation format.name$");
		}
		Object o1 = stack.pop();
		Object o2 = stack.pop();
		Object o3 = stack.pop();

		if (!(o1 instanceof String) && !(o2 instanceof Integer) && !(o3 instanceof String)) {
			
			stack.push("");
			return;
		}
		
		String format = (String) o1;
		Integer name = (Integer) o2;
		String names = (String) o3;
		
		if (names != null){
			AuthorList a = AuthorList.getAuthorList(names);
			if (name.intValue() > a.size()){
				throw new VMException("Author Out of Bounds. Number " + name + " invalid for " + names);
			}
			Author author = a.getAuthor(name.intValue() - 1);
			
			stack.push(BibtexNameFormatter.formatName(author, format, vm));
		} else {
			stack.push("");
		}
	}



}
