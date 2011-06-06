

package org.gjt.sp.jedit.visitors;

import org.gjt.sp.jedit.EditPane;


public class SaveCaretInfoVisitor extends JEditVisitorAdapter
{
	@Override
	public void visit(EditPane editPane)
	{
		editPane.saveCaretInfo();
	}
}
