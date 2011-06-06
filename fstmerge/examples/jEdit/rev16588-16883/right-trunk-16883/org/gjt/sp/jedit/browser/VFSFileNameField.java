

package org.gjt.sp.jedit.browser;


import java.util.HashSet;
import java.awt.event.*;
import java.awt.*;
import org.gjt.sp.jedit.gui.HistoryTextField;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.MiscUtilities;

import org.gjt.sp.util.Log;



class VFSFileNameField extends HistoryTextField
{
	
	VFSFileNameField(VFSBrowser browser, String model)
	{
		super(model);
		setEnterAddsToHistory(false);

		this.browser = browser;

		Dimension dim = getPreferredSize();
		dim.width = Integer.MAX_VALUE;
		setMaximumSize(dim);

		
		
		final int FORWARD = KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS;
		HashSet<AWTKeyStroke> keys = new HashSet<AWTKeyStroke>(
				getFocusTraversalKeys(FORWARD));
		keys.remove(AWTKeyStroke.getAWTKeyStroke("pressed TAB"));
		setFocusTraversalKeys(FORWARD, keys);
	} 

	
	public void processKeyEvent(KeyEvent evt)
	{
		if(evt.getID() == KeyEvent.KEY_PRESSED)
		{
			String path = getText();

			switch(evt.getKeyCode())
			{
			case KeyEvent.VK_TAB:
				doComplete(path);
				break;
			case KeyEvent.VK_LEFT:
				if ((evt.getModifiers() & KeyEvent.ALT_MASK) > 0)
				{
					browser.previousDirectory();
					evt.consume();
				}
				else
				{
					
					super.processKeyEvent(evt);
				}
				break;
			case KeyEvent.VK_UP:
				if ((evt.getModifiers() & KeyEvent.ALT_MASK)>0)
				{
					String p = browser.getDirectory();
					browser.setDirectory(MiscUtilities.getParentOfPath(p));
					evt.consume();
				}
				else
				{
					browser.getBrowserView().getTable()
					.processKeyEvent(evt);
				}
				break;
			case KeyEvent.VK_RIGHT:
				if ((evt.getModifiers() & KeyEvent.ALT_MASK)>0)
				{
					evt.consume();
					browser.nextDirectory();
				}
				else
					super.processKeyEvent(evt);
				break;
			case KeyEvent.VK_DOWN:
			case KeyEvent.VK_PAGE_UP:
			case KeyEvent.VK_PAGE_DOWN:
				browser.getBrowserView().getTable()
					.processKeyEvent(evt);
				break;
			case KeyEvent.VK_ENTER:
				browser.filesActivated(
					(evt.isShiftDown()
					? VFSBrowser.M_OPEN_NEW_VIEW
					: VFSBrowser.M_OPEN),false);
				setText(null);
				evt.consume();
				break;
			default:
				super.processKeyEvent(evt);
				break;
			}
		}
		else if(evt.getID() == KeyEvent.KEY_TYPED)
		{
			char ch = evt.getKeyChar();
			if(ch > 0x20 && ch != 0x7f && ch != 0xff)
			{
				super.processKeyEvent(evt);
				String path = getText();
				BrowserView view = browser.getBrowserView();
				view.selectNone();

				if(MiscUtilities.getLastSeparatorIndex(path) == -1)
				{
					int mode = browser.getMode();
					
					
					
					
					view.getTable().doTypeSelect(path,
						mode == VFSBrowser
						.CHOOSE_DIRECTORY_DIALOG
						||
						mode == VFSBrowser
						.SAVE_DIALOG);
				}
			}
			else
				super.processKeyEvent(evt);
		}
	} 

	
	private VFSBrowser browser;

	
	public String doComplete(String path, String complete, boolean dirsOnly)
	{
		Log.log(Log.DEBUG,VFSFileNameField.class,
			"doComplete(" + path + "," + complete
			+ "," + dirsOnly);

		for(;;)
		{
			if(complete.length() == 0)
				return path;
			int index = MiscUtilities.getFirstSeparatorIndex(complete);
			if(index == -1)
				return path;

			
			String newPath = VFSFile.findCompletion(path,
				complete.substring(0,index),browser,true);
			if(newPath == null)
				return null;
			path = newPath;
			complete = complete.substring(index + 1);
		}
	} 

	
	private void doComplete(String currentText)
	{
		int index = MiscUtilities.getLastSeparatorIndex(currentText);
		String dir;
		if(index != -1)
			dir = currentText.substring(0,index + 1);
		else
			dir = "";

		if(MiscUtilities.isAbsolutePath(currentText))
		{
			if(dir.startsWith("/"))
				dir = dir.substring(1);
			dir = doComplete(VFSBrowser.getRootDirectory(),dir,false);
			if(dir == null)
				return;
	
			browser.setDirectory(dir);
			VFSManager.waitForRequests();

			if(index == -1)
			{
				if(currentText.startsWith("/"))
					currentText = currentText.substring(1);
			}
			else
				currentText = currentText.substring(index + 1);
		}
		else
		{
			if(dir.length() != 0)
			{
				dir = doComplete(browser.getDirectory(),dir,false);
				if(dir == null)
					return;
	
				browser.setDirectory(dir);
				VFSManager.waitForRequests();
	
				currentText = currentText.substring(index + 1);
			}
		}

		BrowserView view = browser.getBrowserView();
		view.selectNone();
		view.getTable().doTypeSelect(currentText,
			browser.getMode() == VFSBrowser
			.CHOOSE_DIRECTORY_DIALOG);

		String newText;

		VFSFile[] files = view.getSelectedFiles();
		if(files.length == 0)
			newText = currentText;
		else
		{
			String path = files[0].getPath();
			String name = files[0].getName();
			String parent = MiscUtilities.getParentOfPath(path);

			if(MiscUtilities.isAbsolutePath(currentText)
				&& !currentText.startsWith(browser.getDirectory()))
			{
				newText = path;
			}
			else
			{
				if(MiscUtilities.pathsEqual(parent,browser.getDirectory()))
					newText = name;
				else
					newText = path;
			}
		}

		setText(newText);
	} 

	
	private void goToParent()
	{
		String name = MiscUtilities.getFileName(browser.getDirectory());
		String parent = MiscUtilities.getParentOfPath(
			browser.getDirectory());
		browser.setDirectory(parent);

		VFS vfs = VFSManager.getVFSForPath(parent);
		if((vfs.getCapabilities() & VFS.LOW_LATENCY_CAP) != 0)
		{
			VFSManager.waitForRequests();
			setText(name);
			browser.getBrowserView().getTable().doTypeSelect(
				name,browser.getMode() == VFSBrowser
				.CHOOSE_DIRECTORY_DIALOG);
		}
	} 

	
}
