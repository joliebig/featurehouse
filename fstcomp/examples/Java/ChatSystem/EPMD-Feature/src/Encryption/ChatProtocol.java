


import java.util.LinkedList;

public class ChatProtocol {
	private LinkedList encryption = new LinkedList();

	public static String decrypt(LinkedList enc, String s) {
		Iterator it = enc.descendingIterator();
		while (it.hasNext()) {
			TransportEncryption e = (TransportEncryption) it.next();
			s = e.decrypt(s);
		}
		return s;
	}
	
	protected String returnE(String s) {
		Iterator it = encryption.iterator();
		TransportEncryption e = null;
		for ( ; it.hasNext(); ) {
			e = (TransportEncryption) it.next();
			s = e.encrypt(s);
		}
		return s;
	}
	
	protected String handleSpecialAction(String action, String value) {
		String s = original(action, value);
		if (s != null)
			return s;
		if (action.equals("crypto")) {
			TransportEncryption e = EncryptionFactory.getFactory().getEncryption(value);
			if (e != null) {
				String res = returnE(OK_STRING);
				if (encryption.contains(e)) {
					encryption.remove(e);
					System.out.println("removed crypto " + e.getName());
				} else {
					encryption.add(e);
					System.out.println("installed crypto " + e.getName());
				}
				return res;
			}
		}
		return null;
	}
	
	public String preProcess(String inputLine) {
		inputLine = decrypt(encryption, inputLine);
		return inputLine;
	}
}