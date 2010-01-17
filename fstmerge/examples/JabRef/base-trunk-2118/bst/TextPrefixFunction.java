package net.sf.jabref.bst;

import java.util.Stack;

import net.sf.jabref.bst.VM.BstEntry;
import net.sf.jabref.bst.VM.BstFunction;




public class TextPrefixFunction implements BstFunction {

	VM vm;

	public TextPrefixFunction(VM vm) {
		this.vm = vm;
	}

	public void execute(BstEntry context) {
		Stack stack = vm.getStack();

		if (stack.size() < 2) {
			throw new VMException("Not enough operands on stack for operation text.prefix$");
		}
		Object o1 = stack.pop();
		Object o2 = stack.pop();

		if (!(o1 instanceof Integer)) {
			vm.warn("An integer is needed as first parameter to text.prefix$");
			stack.push("");
			return;
		}
		if (!(o1 instanceof String)) {
			vm.warn("A string is needed as second parameter to text.prefix$");
			stack.push("");
			return;
		}
		
		stack.push(BibtexTextPrefix.textPrefix(((Integer) o1).intValue(), (String) o2, vm));
	}
}
