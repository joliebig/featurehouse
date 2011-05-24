
package org.jhotdraw.util; 
import java.io.*; 
import java.awt.Color; 
import java.util.List; 
public  class  StorableInput {
		private StreamTokenizer fTokenizer;

		private List fMap;

		public StorableInput(InputStream stream) {	Reader r = new BufferedReader(new InputStreamReader(stream));	fTokenizer = new StreamTokenizer(r);	fTokenizer.wordChars('$', '$');	fMap = CollectionsFactory.current().createList();	}

		public Storable readStorable() throws IOException {	Storable storable;	String s = readString();	if (s.equals("NULL")) {	return null;	}	if (s.equals("REF")) {	int ref = readInt();	return retrieve(ref);	}	storable = (Storable) makeInstance(s);	map(storable);	storable.read(this);	return storable;	}

		public String readString() throws IOException {	int token = fTokenizer.nextToken();	if (token == StreamTokenizer.TT_WORD || token == '"') {	return fTokenizer.sval;	}	String msg = "String expected in line: " + fTokenizer.lineno();	throw new IOException(msg);	}

		public int readInt() throws IOException {	int token = fTokenizer.nextToken();	if (token == StreamTokenizer.TT_NUMBER) {	return (int) fTokenizer.nval;	}	String msg = "Integer expected in line: " + fTokenizer.lineno();	IOException exception = new IOException(msg);	exception.printStackTrace();	throw new IOException(msg);	}

		public long readLong() throws IOException {	long token = fTokenizer.nextToken();	if (token == StreamTokenizer.TT_NUMBER) {	return (long)fTokenizer.nval;	}	String msg = "Long expected in line: " + fTokenizer.lineno();	IOException exception = new IOException(msg);	throw exception;	}

		public Color readColor() {	return new Color(readInt(), readInt(), readInt());	}

		public double readDouble() throws IOException {	int token = fTokenizer.nextToken();	if (token == StreamTokenizer.TT_NUMBER) {	return fTokenizer.nval;	}	String msg = "Double expected in line: " + fTokenizer.lineno();	throw new IOException(msg);	}

		public boolean readBoolean() throws IOException {	int token = fTokenizer.nextToken();	if (token == StreamTokenizer.TT_NUMBER) {	return ((int) fTokenizer.nval) == 1;	}	String msg = "Integer expected in line: " + fTokenizer.lineno();	throw new IOException(msg);	}

		private Object makeInstance(String className) throws IOException {	try {	Class cl = Class.forName(className);	return cl.newInstance();	}	catch (NoSuchMethodError e) {	throw new IOException("Class " + className	+ " does not seem to have a no-arg constructor");	}	catch (ClassNotFoundException e) {	throw new IOException("No class: " + className);	}	catch (InstantiationException e) {	throw new IOException("Cannot instantiate: " + className);	}	catch (IllegalAccessException e) {	throw new IOException("Class (" + className + ") not accessible");	}	}

		private void map(Storable storable) {	if (!fMap.contains(storable)) {	fMap.add(storable);	}	}

		private Storable retrieve(int ref) {	return (Storable)fMap.get(ref);	}


}
