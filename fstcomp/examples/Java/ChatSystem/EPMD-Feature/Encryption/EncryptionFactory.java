

import java.util.LinkedList;
import java.util.Iterator;

public class EncryptionFactory {
	private LinkedList methods;
	
	protected EncryptionFactory() {
		methods = new EncryptionList().methods;
	}
	
	private static EncryptionFactory factory;
	
	public static EncryptionFactory getFactory() {
		if (factory == null)
			factory = new EncryptionFactory();
		return factory;
	}
	
	public TransportEncryption getEncryption(String name) {
		TransportEncryption e = null;
		for (Iterator it = methods.iterator(); it.hasNext(); ) {
			e = (TransportEncryption)it.next();
			if (name.equals(e.getName()))
				return e;
		}
		return null;
	}
}
