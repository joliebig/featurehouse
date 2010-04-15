

public class Reverse 
{
	public static String crypto(String content) {

		char help[] = content.toCharArray();

		for (int i = 0; i < content.length(); i++) {
			help[help.length - i - 1] = content.charAt(i);
		}

		return String.copyValueOf(help);

	}
}