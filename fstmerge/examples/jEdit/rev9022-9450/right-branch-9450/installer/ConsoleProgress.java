

package installer;


public class ConsoleProgress implements Progress
{

	public void setMaximum(int max)
	{
	}

	public void advance(int value)
	{
	}

	public void done()
	{
		System.out.println("*** Installation complete");
	}

	public void error(String message)
	{
		System.err.println("*** An error occurred: " + message);
	}
}
