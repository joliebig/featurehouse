

package installer;


public interface Progress
{
	public void setMaximum(int max);

	public void advance(int value);

	public void done();

	public void message(String message);
	public void error(String message);
}
