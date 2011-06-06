
package org.gjt.sp.jedit.input;


public class DefaultInputHandlerProvider implements InputHandlerProvider
{

	private AbstractInputHandler inputHandler;
	public DefaultInputHandlerProvider(AbstractInputHandler inputHandler)
	{
		this.inputHandler = inputHandler;
	}

	public AbstractInputHandler getInputHandler()
	{
		return inputHandler;
	}
}
