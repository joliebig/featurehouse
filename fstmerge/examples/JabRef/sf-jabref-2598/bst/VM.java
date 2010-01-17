package net.sf.jabref.bst;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.Map;
import java.util.Stack;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.sf.jabref.AuthorList;
import net.sf.jabref.BibtexDatabase;
import net.sf.jabref.BibtexEntry;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.Tree;



public class VM implements Warn {

	PrintStream out = System.out;

	public class Identifier {
		public String name;

		public Identifier(String name) {
			this.name = name;
		}

		public String getName() {
			return name;
		}
	}

	public class Variable {
		public String name;

		public Variable(String name) {
			this.name = name;
		}

		public String getName() {
			return name;
		}
	}

	public interface BstFunction {
		public void execute(BstEntry context);
	}

	public static final Integer FALSE = new Integer(0);

	public static final Integer TRUE = new Integer(1);

	private HashMap<String,BstFunction> buildInFunctions;

	public File file;

	public VM(File f) throws RecognitionException, IOException {
		this(new ANTLRFileStream(f.getPath()));
		this.file = f;
	}

	public VM(String s) throws RecognitionException {
		this(new ANTLRStringStream(s));
	}

	public static CommonTree charStream2CommonTree(CharStream bst) throws RecognitionException {
		BstLexer lex = new BstLexer(bst);
		CommonTokenStream tokens = new CommonTokenStream(lex);
		BstParser parser = new BstParser(tokens);
		BstParser.program_return r = parser.program();
		return (CommonTree) r.getTree();
	}

	public VM(CharStream bst) throws RecognitionException {
		this(charStream2CommonTree(bst));
	}

	public VM(CommonTree tree) {
		this.tree = tree;

		this.buildInFunctions = new HashMap<String, BstFunction>(37);

		buildInFunctions.put(">", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 2) {
					throw new VMException("Not enough operands on stack for operation >");
				}
				Object o2 = stack.pop();
				Object o1 = stack.pop();

				if (!(o1 instanceof Integer && o2 instanceof Integer)) {
					throw new VMException("Can only compare two integers with >");
				}

				if (o1 == o2) {
					stack.push(VM.FALSE);
					return;
				}

				stack.push(((Integer) o1).compareTo((Integer) o2) > 0 ? VM.TRUE : VM.FALSE);
			}
		});

		buildInFunctions.put("<", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 2) {
					throw new VMException("Not enough operands on stack for operation <");
				}
				Object o2 = stack.pop();
				Object o1 = stack.pop();

				if (!(o1 instanceof Integer && o2 instanceof Integer)) {
					throw new VMException("Can only compare two integers with <");
				}

				if (o1 == o2) {
					stack.push(VM.FALSE);
					return;
				}

				stack.push(((Integer) o1).compareTo((Integer) o2) < 0 ? VM.TRUE : VM.FALSE);

			}
		});

		buildInFunctions.put("=", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 2) {
					throw new VMException("Not enough operands on stack for operation =");
				}
				Object o1 = stack.pop();
				Object o2 = stack.pop();

				if (o1 == null ^ o2 == null) {
					stack.push(VM.FALSE);
					return;
				}

				if (o1 == o2) {
					stack.push(VM.TRUE);
					return;
				}

				stack.push(o1.equals(o2) ? VM.TRUE : VM.FALSE);
			}
		});

		buildInFunctions.put("+", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 2) {
					throw new VMException("Not enough operands on stack for operation +");
				}
				Object o2 = stack.pop();
				Object o1 = stack.pop();

				if (!(o1 instanceof Integer && o2 instanceof Integer)) {
					throw new VMException("Can only compare two integers with +");
				}

				stack.push(new Integer(((Integer) o1).intValue() + ((Integer) o2).intValue()));
			}
		});

		buildInFunctions.put("-", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 2) {
					throw new VMException("Not enough operands on stack for operation -");
				}
				Object o2 = stack.pop();
				Object o1 = stack.pop();

				if (!(o1 instanceof Integer && o2 instanceof Integer)) {
					throw new VMException("Can only subtract two integers with -");
				}

				stack.push(new Integer(((Integer) o1).intValue() - ((Integer) o2).intValue()));
			}
		});

		buildInFunctions.put("*", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 2) {
					throw new VMException("Not enough operands on stack for operation *");
				}
				Object o2 = stack.pop();
				Object o1 = stack.pop();

				if (!(o1 instanceof String && o2 instanceof String)) {
					throw new VMException("Can only concatenate two String with *");
				}

				stack.push(((String) o1) + ((String) o2));
			}
		});

		buildInFunctions.put(":=", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 2) {
					throw new VMException("Invalid call to operation :=");
				}
				Object o1 = stack.pop();
				Object o2 = stack.pop();
				assign(context, o1, o2);

			}
		});

		buildInFunctions.put("add.period$", new BstFunction() {

			Pattern p = Pattern.compile("([^\\.\\?\\!\\}\\s])(\\}|\\s)*$");

			
			public void execute(BstEntry context) {
				if (stack.size() < 1) {
					throw new VMException("Not enough operands on stack for operation add.period$");
				}
				Object o1 = stack.pop();

				if (!(o1 instanceof String)) {
					throw new VMException("Can only add a period to a string for add.period$");
				}

				String s = (String) o1;
				Matcher m = p.matcher(s);

				if (m.find()) {
					StringBuffer sb = new StringBuffer();
					m.appendReplacement(sb, m.group(1));
					sb.append('.');
					String group2 = m.group(2);
					if (group2 != null)
						sb.append(m.group(2));
					stack.push(sb.toString());
				} else {
					stack.push(s);
				}
			}
		});

		buildInFunctions.put("call.type$", new BstFunction() {
			
			public void execute(BstEntry context) {

				if (context == null) {
					throw new VMException(
						"Call.type$ can only be called from within a context (ITERATE or REVERSE).");
				}
				VM.this.execute(context.entry.getType().getName().toLowerCase(), context);
			}
		});

		buildInFunctions.put("change.case$", new ChangeCaseFunction(this));

		buildInFunctions.put("chr.to.int$", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 1) {
					throw new VMException("Not enough operands on stack for operation chr.to.int$");
				}
				Object o1 = stack.pop();

				if (!(o1 instanceof String && ((String) o1).length() == 1)) {
					throw new VMException("Can only perform chr.to.int$ on string with length 1");
				}

				String s = (String) o1;

				stack.push(new Integer(s.charAt(0)));
			}
		});

		buildInFunctions.put("cite$", new BstFunction() {
			
			public void execute(BstEntry context) {
				stack.push(context.entry.getCiteKey());
			}
		});

		buildInFunctions.put("duplicate$", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 1) {
					throw new VMException("Not enough operands on stack for operation duplicate$");
				}
				Object o1 = stack.pop();

				stack.push(o1);
				stack.push(o1);
			}
		});

		buildInFunctions.put("empty$", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 1) {
					throw new VMException("Not enough operands on stack for operation empty$");
				}
				Object o1 = stack.pop();

				if (o1 == null) {
					stack.push(VM.TRUE);
					return;
				}

				if (!(o1 instanceof String)) {
					throw new VMException("Operand does not match function empty$");
				}

				String s = (String) o1;

				stack.push(s.trim().equals("") ? VM.TRUE : VM.FALSE);
			}
		});

		buildInFunctions.put("format.name$", new FormatNameFunction(this));

		buildInFunctions.put("if$", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 3) {
					throw new VMException("Not enough operands on stack for operation =");
				}
				Object f1 = stack.pop();
				Object f2 = stack.pop();
				Object i = stack.pop();

				if (!(f1 instanceof Identifier || f1 instanceof Tree)
					&& (f2 instanceof Identifier || f2 instanceof Tree) && (i instanceof Integer)) {
					throw new VMException("Expecting two functions and an integer for if$.");
				}

				Object toExe;
				if (((Integer) i).intValue() > 0) {
					toExe = f2;
				} else {
					toExe = f1;
				}
				VM.this.executeInContext(toExe, context);
			}
		});

		buildInFunctions.put("int.to.chr$", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 1) {
					throw new VMException("Not enough operands on stack for operation int.to.chr$");
				}
				Object o1 = stack.pop();

				if (!(o1 instanceof Integer)) {
					throw new VMException("Can only perform operation int.to.chr$ on an Integer");
				}

				Integer i = (Integer) o1;

				stack.push(String.valueOf((char) i.intValue()));
			}
		});

		buildInFunctions.put("int.to.str$", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 1) {
					throw new VMException("Not enough operands on stack for operation int.to.str$");
				}
				Object o1 = stack.pop();

				if (!(o1 instanceof Integer)) {
					throw new VMException(
						"Can only transform an integer to an string using int.to.str$");
				}

				stack.push(((Integer) o1).toString());
			}
		});

		buildInFunctions.put("missing$", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 1) {
					throw new VMException("Not enough operands on stack for operation missing$");
				}
				Object o1 = stack.pop();

				if (o1 == null) {
					stack.push(VM.TRUE);
					return;
				}

				if (!(o1 instanceof String)) {
					warn("Not a string or missing field in operation missing$");
					stack.push(VM.TRUE);
					return;
				}

				stack.push(VM.FALSE);
			}
		});

		buildInFunctions.put("newline$", new BstFunction() {
			
			public void execute(BstEntry context) {
				VM.this.bbl.append('\n');
			}
		});

		buildInFunctions.put("num.names$", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 1) {
					throw new VMException("Not enough operands on stack for operation num.names$");
				}
				Object o1 = stack.pop();

				if (!(o1 instanceof String)) {
					throw new VMException("Need a string at the top of the stack for num.names$");
				}
				String s = (String) o1;

				stack.push(new Integer(AuthorList.getAuthorList(s).size()));
			}
		});

		buildInFunctions.put("pop$", new BstFunction() {
			
			public void execute(BstEntry context) {
				stack.pop();
			}
		});

		buildInFunctions.put("preamble$", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (preamble != null) {
					stack.push(preamble);
				} else {
					stack.push("");
				}

			}
		});

		
		buildInFunctions.put("purify$", new PurifyFunction(this));

		buildInFunctions.put("quote$", new BstFunction() {
			
			public void execute(BstEntry context) {
				stack.push("\"");
			}
		});

		buildInFunctions.put("skip$", new BstFunction() {
			
			public void execute(BstEntry context) {
				
			}
		});

		buildInFunctions.put("stack$", new BstFunction() {
			
			public void execute(BstEntry context) {
				while (!stack.empty()) {
					System.out.println(stack.pop());
				}
			}
		});

		buildInFunctions.put("substring$", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 3) {
					throw new VMException("Not enough operands on stack for operation substring$");
				}
				Object o1 = stack.pop();
				Object o2 = stack.pop();
				Object o3 = stack.pop();

				if (!((o1 instanceof Integer) && (o2 instanceof Integer) && (o3 instanceof String))) {
					throw new VMException("Expecting two integers and a string for substring$");
				}

				Integer len = (Integer) o1;
				Integer start = (Integer) o2;

				int lenI = len.intValue();
				int startI = start.intValue();

				if (lenI > Integer.MAX_VALUE / 2)
					lenI = Integer.MAX_VALUE / 2;

				if (startI > Integer.MAX_VALUE / 2)
					startI = Integer.MAX_VALUE / 2;

				if (startI < Integer.MIN_VALUE / 2)
					startI = -Integer.MIN_VALUE / 2;

				String s = (String) o3;

				if (startI < 0) {
					startI += s.length() + 1;
					startI = Math.max(1, startI + 1 - lenI);
				}
				stack.push(s.substring(startI - 1, Math.min(startI - 1 + lenI, s.length())));
			}
		});

		buildInFunctions.put("swap$", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 2) {
					throw new VMException("Not enough operands on stack for operation swap$");
				}
				Object f1 = stack.pop();
				Object f2 = stack.pop();

				stack.push(f1);
				stack.push(f2);
			}
		});

		buildInFunctions.put("text.length$", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 1) {
					throw new VMException("Not enough operands on stack for operation text.length$");
				}
				Object o1 = stack.pop();

				if (!(o1 instanceof String)) {
					throw new VMException("Can only perform operation on a string text.length$");
				}

				String s = (String) o1;
				char[] c = s.toCharArray();
				int result = 0;

				

				
				int i = 0;

				
				int n = s.length();

				
				int braceLevel = 0;

				
				while (i < n) {
					
					i++;
					
					
					if (c[i - 1] == '{') {
						
						braceLevel++;
						
						if (braceLevel == 1 && i < n)
							
							
							if (c[i] == '\\') {
								
								i++; 
								
								
								while (i < n && braceLevel > 0) {
									
									if (c[i] == '}')
										
										braceLevel--;
									
									
									else if (c[i] == '{')

										
										braceLevel++;
									
									i++;
									
								}
								
								result++;
								
							}
						
					}
					
					
					else if (c[i - 1] == '}') {
						
						if (braceLevel > 0)
							
							braceLevel--;
						
					}
					
					else
						
						result++;
				}
				stack.push(new Integer(result));
			}
		});

		
		buildInFunctions.put("text.prefix$", new TextPrefixFunction(this));

		buildInFunctions.put("top$", new BstFunction() {
			
			public void execute(BstEntry context) {
				System.out.println(stack.pop());
			}
		});

		buildInFunctions.put("type$", new BstFunction() {
			
			public void execute(BstEntry context) {
				stack.push(context.entry.getType().getName());
			}
		});

		buildInFunctions.put("warning$", new BstFunction() {
			
			int warning = 1;

			public void execute(BstEntry context) {
				out.println("Warning (#" + (warning++) + "): " + stack.pop());
			}
		});

		buildInFunctions.put("while$", new BstFunction() {
			
			public void execute(BstEntry context) {
				if (stack.size() < 2) {
					throw new VMException("Not enough operands on stack for operation while$");
				}
				Object f2 = stack.pop();
				Object f1 = stack.pop();

				if (!(f1 instanceof Identifier || f1 instanceof Tree)
					&& (f2 instanceof Identifier || f2 instanceof Tree)) {
					throw new VMException("Expecting two functions for while$.");
				}

				do {
					VM.this.executeInContext(f1, context);

					Object i = stack.pop();
					if (!(i instanceof Integer)) {
						throw new VMException(
							"First parameter to while has to return an integer but was " + i);
					}
					if (((Integer) i).intValue() <= 0) {
						break;
					}
					VM.this.executeInContext(f2, context);
				} while (true);
			}
		});

		buildInFunctions.put("width$", new WidthFunction(this));

		buildInFunctions.put("write$", new BstFunction() {
			
			public void execute(BstEntry context) {
				String s = (String) stack.pop();
				System.out.println(s);
				VM.this.bbl.append(s);
			}
		});

	}

	protected boolean assign(BstEntry context, Object o1, Object o2) {

		if (!(o1 instanceof Identifier) || !(o2 instanceof String || o2 instanceof Integer))
			throw new VMException("Invalid parameters");

		String name = ((Identifier) o1).getName();

		if (o2 instanceof String) {

			if (context != null && context.strings.containsKey(name)) {
				context.strings.put(name, (String) o2);
				return true;
			}

			if (strings.containsKey(name)) {
				strings.put(name, (String) o2);
				return true;
			}
			return false;

		}
		if (o2 instanceof Integer) {
			if (context != null && context.integers.containsKey(name)) {
				context.integers.put(name, (Integer) o2);
				return true;
			}

			if (integers.containsKey(name)) {
				integers.put(name, (Integer) o2);
				return true;
			}
			return false;
		}
		return false;
	}

	CommonTree tree;

	private StringBuffer bbl;

	String preamble;

	public String run(BibtexDatabase db) {
		preamble = db.getPreamble();
		return run(db.getEntries());
	}

	public String run(Collection<BibtexEntry> bibtex) {

		reset();

		{ 
			entries = new Vector<BstEntry>(bibtex.size());
			ListIterator<BstEntry> i = entries.listIterator();
			for (BibtexEntry entry : bibtex){
				i.add(new BstEntry(entry));
			}
		}

		

		
		for (int i = 0; i < tree.getChildCount(); i++) {
			Tree child = tree.getChild(i);
			switch (child.getType()) {
			case BstParser.STRINGS:
				strings(child);
				break;
			case BstParser.INTEGERS:
				integers(child);
				break;
			case BstParser.FUNCTION:
				function(child);
				break;
			case BstParser.EXECUTE:
				execute(child);
				break;
			case BstParser.SORT:
				sort(child);
				break;
			case BstParser.ITERATE:
				iterate(child);
				break;
			case BstParser.REVERSE:
				reverse(child);
				break;
			case BstParser.ENTRY:
				entry(child);
				break;
			case BstParser.READ:
				read();
				break;
			case BstParser.MACRO:
				macro(child);
				break;
			}
		}

		return bbl.toString();
	}

	private void reset() {
		bbl = new StringBuffer();

		entries = null;

		strings = new HashMap<String, String>();

		integers = new HashMap<String, Integer>();
		integers.put("entry.max$", new Integer(Integer.MAX_VALUE));
		integers.put("global.max$", new Integer(Integer.MAX_VALUE));

		functions = new HashMap<String, BstFunction>();
		functions.putAll(buildInFunctions);

		stack = new Stack<Object>();
	}

	
	private void read() {

		Iterator<BstEntry> i = entries.iterator();
		while (i.hasNext()) {
			BstEntry e = i.next();

			for (Map.Entry<String, String> mEntry : e.fields.entrySet()){
				Object fieldValue = e.entry.getField(mEntry.getKey());

				mEntry.setValue((fieldValue == null ? null : fieldValue.toString()));
			}
		}

		i = entries.iterator();
		while (i.hasNext()) {
			BstEntry e = i.next();
			if (!e.fields.containsKey("crossref")) {
				e.fields.put("crossref", null);
			}
		}
	}

	
	private void macro(Tree child) {
		String name = child.getChild(0).getText();
		String replacement = child.getChild(1).getText();
		functions.put(name, new MacroFunction(replacement));
	}

	public class MacroFunction implements BstFunction {

		String replacement;

		public MacroFunction(String replacement) {
			this.replacement = replacement;
		}

		public void execute(BstEntry context) {
			VM.this.push(replacement);
		}
	}

	
	private void entry(Tree child) {

		{ 
			Tree t = child.getChild(0);
			

			for (int i = 0; i < t.getChildCount(); i++) {
				String name = t.getChild(i).getText();

				for (BstEntry entry : entries){
					entry.fields.put(name, null);
				}
			}
		}
		{ 
			Tree t = child.getChild(1);
			

			for (int i = 0; i < t.getChildCount(); i++) {
				String name = t.getChild(i).getText();
				
				for (BstEntry entry : entries){
					entry.integers.put(name, new Integer(0));
				}
			}
		}
		{ 
			Tree t = child.getChild(2);
			

			for (int i = 0; i < t.getChildCount(); i++) {
				String name = t.getChild(i).getText();
				for (BstEntry entry : entries){
					entry.strings.put(name, null);
				}
			}
			for (BstEntry entry : entries){
				entry.strings.put("sort.key$", null);
			}
		}
	}

	private void reverse(Tree child) {

		BstFunction f = functions.get(child.getChild(0).getText());

		ListIterator<BstEntry> i = entries.listIterator(entries.size());
		while (i.hasPrevious()) {
			f.execute(i.previous());
		}
	}

	private void iterate(Tree child) {
		BstFunction f = functions.get(child.getChild(0).getText());

		Iterator<BstEntry> i = entries.iterator();
		while (i.hasNext()) {
			f.execute(i.next());
		}
	}

	
	private void sort(Tree child) {
		Collections.sort(entries, new Comparator<BstEntry>() {
			public int compare(BstEntry o1, BstEntry o2) {
				return (o1.strings.get("sort.key$")).compareTo(o2.strings
					.get("sort.key$"));
			}
		});
	}

	public void executeInContext(Object o, BstEntry context) {
		if (o instanceof Tree) {
			Tree t = (Tree) o;
			new StackFunction(t).execute(context);
		} else if (o instanceof Identifier) {
			execute(((Identifier) o).getName(), context);
		}
	}

	public void execute(Tree child) {
		execute(child.getChild(0).getText(), null);
	}

	public class StackFunction implements BstFunction {

		Tree tree;

		public Tree getTree() {
			return tree;
		}

		public StackFunction(Tree stack) {
			
			tree = stack;
		}

		public void execute(BstEntry context) {

			for (int i = 0; i < tree.getChildCount(); i++) {

				Tree c = tree.getChild(i);
				try {

					switch (c.getType()) {
					case BstParser.STRING: {
						String s = c.getText();
						push(s.substring(1, s.length() - 1));
					}
						break;
					case BstParser.INTEGER:
						push(new Integer(Integer.parseInt(c.getText().substring(1))));
						break;
					case BstParser.QUOTED:
						push(new Identifier(c.getText().substring(1)));
						break;
					case BstParser.STACK:
						push(c);
						break;
					default:
						VM.this.execute(c.getText(), context);
					}
				} catch (VMException e) {
					if (file != null) {
						System.err.println("ERROR " + e.getMessage() + " (" + file.getPath() + ":"
							+ c.getLine() + ")");
					} else {
						System.err.println("ERROR " + e.getMessage() + " (" + c.getLine() + ")");
					}
					throw e;
				}
			}

		}
	}

	private void push(Tree t) {
		stack.push(t);
	}

	public void execute(String name, BstEntry context) {

		if (context != null) {

			if (context.fields.containsKey(name)) {
				stack.push(context.fields.get(name));
				return;
			}
			if (context.strings.containsKey(name)) {
				stack.push(context.strings.get(name));
				return;
			}
			if (context.integers.containsKey(name)) {
				stack.push(context.integers.get(name));
				return;
			}
		}
		if (strings.containsKey(name)) {
			stack.push(strings.get(name));
			return;
		}
		if (integers.containsKey(name)) {
			stack.push(integers.get(name));
			return;
		}

		if (functions.containsKey(name)) {
			functions.get(name).execute(context);
			return;
		}

		throw new VMException("No matching identifier found: " + name);
	}

	private void function(Tree child) {
		String name = child.getChild(0).getText();
		Tree stack = child.getChild(1);
		functions.put(name, new StackFunction(stack));

	}

	
	private void integers(Tree child) {
		Tree t = child.getChild(0);
		

		for (int i = 0; i < t.getChildCount(); i++) {
			String name = t.getChild(i).getText();
			integers.put(name, new Integer(0));
		}
	}

	
	private void strings(Tree child) {
		Tree t = child.getChild(0);
		

		for (int i = 0; i < t.getChildCount(); i++) {
			String name = t.getChild(i).getText();
			strings.put(name, null);
		}
	}

	public class BstEntry {

		public BstEntry(BibtexEntry e) {
			this.entry = e;
		}

		BibtexEntry entry;

		Map<String, String> strings = new HashMap<String, String>();

		Map<String, String> fields = new HashMap<String, String>();

		Map<String, Integer> integers = new HashMap<String, Integer>();

		public Map<String, String> getFields() {
			return fields;
		}

		public BibtexEntry getBibtexEntry() {
			return entry;
		}
	}

	Vector<BstEntry> entries;
	
	Map<String, String> strings = new HashMap<String, String>();
	
	Map<String, Integer> integers = new HashMap<String, Integer>();
	
	Map<String, BstFunction> functions = new HashMap<String, BstFunction>();
	
	Stack<Object> stack = new Stack<Object>();
	
	public void push(Integer integer) {
		stack.push(integer);
	}

	public void push(String string) {
		stack.push(string);
	}

	public void push(Identifier identifier) {
		stack.push(identifier);
	}

	
	  public Map<String, String> getStrings() { return strings; }
	  
	  public Map<String, Integer> getIntegers() { return integers; }
	  
	  public Vector<BstEntry> getEntries() { return entries; }
	  
	 public Map<String, BstFunction> getFunctions() { return functions; }

	public Stack<Object> getStack() {
		return stack;
	}

	public void warn(String string) {
		System.out.println(string);
	}

}
