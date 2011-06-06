

package org.gjt.sp.jedit.gui;


import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.PrintWriter;
import java.io.StringWriter;
import org.gjt.sp.jedit.*;



public class BeanShellErrorDialog extends TextAreaDialog
{
	public BeanShellErrorDialog(Frame frame, Throwable t)
	{
		super(frame,jEdit.getProperty("beanshell-error.title"),
			jEdit.getProperty("beanshell-error.message"),
			UIManager.getIcon("OptionPane.errorIcon"),
			MiscUtilities.throwableToString(t));
	}

	
	public BeanShellErrorDialog(View view, Throwable t)
	{
		this((Frame)view,t);
	}
}
