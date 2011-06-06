

package org.gjt.sp.jedit.gui;


import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import org.gjt.sp.jedit.*;



public class BeanShellErrorDialog extends TextAreaDialog
{
	public BeanShellErrorDialog(Frame frame, Throwable t)
	{
		super(frame,"beanshell-error",t);
	}

	
	public BeanShellErrorDialog(View view, Throwable t)
	{
		this((Frame)view,t);
	}
}
