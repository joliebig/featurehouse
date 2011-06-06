

package org.gjt.sp.jedit.browser;


import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.io.File;
import org.gjt.sp.jedit.gui.HistoryTextField;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.OperatingSystem;



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

		ActionMap map = getActionMap();
		Action backspace = map.get("delete-previous");
		map.put("delete-previous",new BackspaceAction(backspace));
	} 

	
	public boolean isManagingFocus()
	{
		return false;
	} 

	
	public boolean getFocusTraversalKeysEnabled()
	{
		return false;
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
				if(getCaretPosition() == 0)
					browser.getBrowserView().getTable().processKeyEvent(evt);
				else
					super.processKeyEvent(evt);
				break;
			case KeyEvent.VK_RIGHT:
				if(getCaretPosition() == getDocument().getLength())
					browser.getBrowserView().getTable().processKeyEvent(evt);
				else
					super.processKeyEvent(evt);
				break;
			case KeyEvent.VK_UP:
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
			if(ch == '/' || ch == File.separatorChar)
			{
				super.processKeyEvent(evt);
				String path = getText();

				if(path.length() == 2 && path.charAt(0) == '-')
				{
					path = browser.getView().getBuffer()
						.getDirectory();
				}
				else if(path.length() == 3 && path.startsWith(".."))
				{
					path = MiscUtilities.getParentOfPath(
						browser.getDirectory());
					VFS vfs = VFSManager.getVFSForPath(path);
					if((vfs.getCapabilities() & VFS.LOW_LATENCY_CAP) != 0)
					{
						browser.setDirectory(path);
						VFSManager.waitForRequests();
						setText(null);
					}
				}
				else if(!MiscUtilities.isAbsolutePath(path))
				{
					VFS.DirectoryEntry[] files = browser
						.getBrowserView().getSelectedFiles();
					if(files.length != 1
						|| files[0].type ==
						VFS.DirectoryEntry.FILE)
					{
						return;
					}
					path = files[0].path;
				}
				else if(OperatingSystem.isDOSDerived()
					&& path.length() == 3
					&& path.charAt(1) == ':')
				{
					browser.setDirectory(path);
					VFSManager.waitForRequests();
					setText(null);
				}

				VFS vfs = VFSManager.getVFSForPath(path);
				if((vfs.getCapabilities() & VFS.LOW_LATENCY_CAP) != 0)
				{
					setText(null);
					browser.setDirectory(path);
					VFSManager.waitForRequests();
				}
				else
				{
					if(path.endsWith("/") || path.endsWith(File.separator))
						setText(path);
					else
						setText(path + vfs.getFileSeparator());
				}
			}
			else if(ch > 0x20 && ch != 0x7f && ch != 0xff)
			{
				super.processKeyEvent(evt);
				String path = getText();

				BrowserView view = browser.getBrowserView();
				view.selectNone();

				int mode = browser.getMode();
				
				
				
				
				view.getTable().doTypeSelect(path,
					mode == VFSBrowser
					.CHOOSE_DIRECTORY_DIALOG
					||
					mode == VFSBrowser
					.SAVE_DIALOG);
			}
			else
				super.processKeyEvent(evt);
		}
	} 

	
	private VFSBrowser browser;

	
	private void doComplete(String currentText)
	{
		BrowserView view = browser.getBrowserView();
		view.selectNone();
		view.getTable().doTypeSelect(currentText,
			browser.getMode() == VFSBrowser
			.CHOOSE_DIRECTORY_DIALOG);

		VFS.DirectoryEntry[] files = view.getSelectedFiles();
		if(files.length == 0)
			return;

		String path = files[0].path;
		String name = files[0].name;
		String parent = MiscUtilities.getParentOfPath(path);

		String newText;
		if(MiscUtilities.isAbsolutePath(currentText)
			&& !currentText.startsWith(browser.getDirectory()))
		{
			newText = path;
		}
		else
		{
			if(VFSBrowser.pathsEqual(parent,browser.getDirectory()))
				newText = name;
			else
				newText = path;
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

	

	
	
	class BackspaceAction extends AbstractAction
	{
		private Action delegate;

		BackspaceAction(Action delegate)
		{
			this.delegate = delegate;
		}

		public void actionPerformed(ActionEvent evt)
		{
			if(getSelectionStart() == 0
				&& getSelectionEnd() == 0)
			{
				goToParent();
			}
			else
			{
				delegate.actionPerformed(evt);

				String path = getText();

				BrowserView view = browser.getBrowserView();
				view.selectNone();
				view.getTable().doTypeSelect(path,
					browser.getMode() == VFSBrowser
					.CHOOSE_DIRECTORY_DIALOG);
			}
		}
	} 
}
