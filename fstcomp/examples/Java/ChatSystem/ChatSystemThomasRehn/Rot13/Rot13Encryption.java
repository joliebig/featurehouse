

public class Rot13Encryption extends TransportEncryption {
	public Rot13Encryption() {
		super("rot13");
	}
	
	public String encrypt(String s) {
		StringBuffer res = new StringBuffer();
		char[] cs = s.toCharArray();
		for (int i = 0; i < cs.length; ++i) {
			char c = cs[i];
			if (Character.isLowerCase(c)) {
				res.append((char)((((c - 97) + 13) % 26) + 97));
			} else if (Character.isUpperCase(c)) {
				res.append((char)((((c - 65) + 13) % 26) + 65));
			} else {
				res.append(c);
			}
		}
		return res.toString();
	}
	
	public String decrypt(String s) {
		return encrypt(s);
	}
}
