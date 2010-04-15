

public class ReverseEncryption extends TransportEncryption {

	public ReverseEncryption() {
		super("reverse");
	}
	
	public String encrypt(String s) {
		return new StringBuffer(s).reverse().toString();
	}
	
	public String decrypt(String s) {
		return encrypt(s);
	}
}
