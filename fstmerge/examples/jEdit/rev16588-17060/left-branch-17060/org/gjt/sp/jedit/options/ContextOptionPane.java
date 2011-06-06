

package org.gjt.sp.jedit.options;

import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.util.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.StandardUtilities;


public class ContextOptionPane extends AbstractContextOptionPane
{

	public ContextOptionPane()
	{
		super("context", jEdit.getProperty("options.context.caption"));
	}

    
    protected String getContextMenu()
    {
		return jEdit.getProperty("view.context");
	}

    
    protected void saveContextMenu(String menu)
    {
		jEdit.setProperty("view.context", menu);
    }

}
