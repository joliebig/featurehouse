

public class SwitchTwo 
{
	public static String crypto(String content)
	{
		char helper;

		char[] help = content.toCharArray();

		for (int i = 0; i < help.length - 1; i = i + 2) {

			helper = help[i];
			help[i] = help[i + 1];
			help[i + 1] = helper;
		}

		return String.copyValueOf(help);
		
	}
}