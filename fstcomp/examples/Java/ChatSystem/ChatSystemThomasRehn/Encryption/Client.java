

import java.util.Iterator;

public class Client {
	private LinkedList encryption = new LinkedList();

	protected void send(String s) {
		TransportEncryption e = null;
		for (Iterator it = encryption.iterator(); it.hasNext(); ) {
			e = (TransportEncryption)it.next();
			s = e.encrypt(s);
		}
		System.out.println(" > sent " + s);
		System.out.flush();
		out.println(s);
	}
	
	protected String read() throws IOException {
		String line = in.readLine();
		line = ChatProtocol.decrypt(encryption, line).replace("\\n", "\n");
		System.out.println(" > read " + line);
		System.out.flush();
		return line;
	}
	
	public boolean addEncryption(TransportEncryption enc) throws IOException {
		if (sendCommand("crypto", enc.getName())) {
			if (!encryption.contains(enc)) {
				encryption.add(enc);
				System.out.println("installed " + enc.getName());
			}
			System.out.println("crypto chain length " + encryption.size());
			return true;
		}
		return false;
	}
	
	public boolean removeEncryption(TransportEncryption enc) throws IOException {
		if (sendCommand("crypto", enc.getName())) {
			if (encryption.contains(enc)) {
				encryption.remove(enc);
				System.out.println("removed " + enc.getName());
			}
			System.out.println("crypto chain length " + encryption.size());
			return true;
		}
		return false;
	}
	
}