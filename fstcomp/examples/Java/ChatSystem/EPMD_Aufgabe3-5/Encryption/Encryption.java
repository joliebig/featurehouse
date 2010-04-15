

public class Encryption {
	public static String encrptMethod1(String msg){
		int i, len = msg.length();
	    StringBuffer dest = new StringBuffer(len);

	    for (i = (len - 1); i >= 0; i--)
	      dest.append(msg.charAt(i));
	    return dest.toString();
	}
	
	public static String encrptMethod2(String msg){
		char temp1,temp2;
		String retString = msg;
		if (msg.length()< 2){
			return retString;
		}else{
			temp1 = msg.charAt(msg.length() - 1);
			temp2 = msg.charAt(msg.length() - 2);
			retString = msg.substring(0, msg.length()- 2) + Character.toString(temp1) + Character.toString(temp2);
			return retString;
		}
	}	
}