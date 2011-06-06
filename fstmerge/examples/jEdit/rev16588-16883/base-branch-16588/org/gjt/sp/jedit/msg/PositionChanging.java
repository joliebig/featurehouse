package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.EditPane;
import org.gjt.sp.jedit.textarea.TextArea;



public class PositionChanging extends EditPaneUpdate
{
	
	protected PositionChanging(EditPane editPane, Object whatt)
	{
		super(editPane, whatt);
	}

	public PositionChanging(TextArea textArea)
	{
		super(EditPane.get(textArea), EditPaneUpdate.POSITION_CHANGING);
	}
	
	public PositionChanging(EditPane editPane)
	{
		super (editPane, EditPaneUpdate.POSITION_CHANGING);
	}
}
