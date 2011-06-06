

package org.gjt.sp.jedit.textarea;


import javax.swing.*;
import java.awt.datatransfer.*;
import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.net.URI;
import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.EditPane;
import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.util.Log;


public class TextAreaTransferHandler extends TransferHandler
{
	
	private static JEditTextArea dragSource;
	private static boolean compoundEdit;
	private static boolean sameTextArea;
	private static int insertPos;
	private static int insertOffset;
	
	
	

	
	protected Transferable createTransferable(JComponent c)
	{
		Log.log(Log.DEBUG,this,"createTransferable()");
		JEditTextArea textArea = (JEditTextArea)c;
		if(textArea.getSelectionCount() == 0)
			return null;
		else
		{
			dragSource = textArea;
			return new TextAreaSelection(textArea);
		}
	} 

	
	public int getSourceActions(JComponent c)
	{
		return COPY_OR_MOVE;
	} 

	
	public boolean importData(JComponent c, Transferable t)
	{
		Log.log(Log.DEBUG,this,"Import data");

		if(!canImport(c,t.getTransferDataFlavors()))
			return false;

		boolean returnValue;

		try
		{
			if(t.isDataFlavorSupported(DataFlavor.javaFileListFlavor))
			{
				returnValue = importFile(c,t);
			}
			else {
				DataFlavor	uriListStringDataFlavor = null;
				DataFlavor[]	dataFlavors		= t.getTransferDataFlavors();
				
				for (int i = 0;i<dataFlavors.length;i++) {
					DataFlavor dataFlavor = dataFlavors[i];
					if (dataFlavor.getPrimaryType().equals("text")&&dataFlavor.getSubType().equals("uri-list")&&(dataFlavor.getRepresentationClass()==String.class)) {
						uriListStringDataFlavor = dataFlavor;
						break;
					}
 				}
				
				if ((uriListStringDataFlavor!=null)&&t.isDataFlavorSupported(uriListStringDataFlavor))
				{
					returnValue = importURIList(c,t,uriListStringDataFlavor);
				}
				else
				{
					returnValue = importText(c,t);
				}
			}
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,this,e);
			returnValue = false;
		}

		GUIUtilities.getView(c).toFront();
		GUIUtilities.getView(c).requestFocus();
		c.requestFocus();

		return returnValue;
	} 

	
	private boolean importFile(JComponent c, Transferable t)
		throws Exception
	{
		Log.log(Log.DEBUG,this,"=> File list");
		EditPane editPane = (EditPane)
			GUIUtilities.getComponentParent(
			c,EditPane.class);

		Buffer buffer = null;

		Object data = t.getTransferData(
			DataFlavor.javaFileListFlavor);

		Iterator iterator = ((List)data)
			.iterator();

		while(iterator.hasNext())
		{
			File file = (File)
				iterator.next();
			Buffer _buffer = jEdit.openFile(null,
				file.getPath());
			if(_buffer != null)
				buffer = _buffer;
		}

		if(buffer != null)
			editPane.setBuffer(buffer);
		editPane.getView().toFront();
		editPane.getView().requestFocus();
		editPane.requestFocus();

		return true;
	} 

	
	private boolean importURIList(JComponent c, Transferable t,DataFlavor uriListStringDataFlavor)
		throws Exception
	{
		String str = (String) t.getTransferData(uriListStringDataFlavor);

		Log.log(Log.DEBUG,this,"=> URIList \""+str+"\"");
		
		JEditTextArea textArea = (JEditTextArea) c;
		if (dragSource == null)
		{
			String		maybeManyFileURI	= str;
			boolean		found			= false;			
			String[]	components		= str.split("\r\n");
			
			for (int i = 0;i<components.length;i++)
			{
				String	str0	= components[i];
				
				if (str0.length()>0) {
					URI	uri	= new URI(str0); 
					
					if ("file".equals(uri.getScheme()))
					{
						org.gjt.sp.jedit.io.VFSManager.runInWorkThread(new DraggedURLLoader(textArea,uri.getPath()));
						found = true;
					}
					else
					{
						Log.log(Log.DEBUG,this,"I do not know how to handle this URI "+uri+", ignoring.");
					}
				}
				else
				{
					
					if (i!=components.length-1)
					{
						Log.log(Log.DEBUG,this,"Odd: there is an empty line in the uri list which is not the last line.");
					}
				}
			}
			
			if (found)
			{
				return true;
			}
		}
		
		return true;
	}
	
	
	private boolean importText(JComponent c, Transferable t)
		throws Exception
	{
		String str = (String)t.getTransferData(
			DataFlavor.stringFlavor);
		str = str.trim();
		Log.log(Log.DEBUG,this,"=> String \""+str+"\"");
		
		JEditTextArea textArea = (JEditTextArea)c;
		if (dragSource == null)
		{
			String		maybeManyFileNames	= str;
			boolean		found			= false;			
			String[]	components		= str.split("\n");
			
			for (int i = 0;i<components.length;i++)
			{
				String str0 = components[i];
				
				
				org.gjt.sp.jedit.io.VFS vfs = org.gjt.sp.jedit.io.VFSManager.getVFSForPath(str0);
				if (!(vfs instanceof org.gjt.sp.jedit.io.FileVFS) || str.startsWith("file://"))
				{
				

					if (str0.startsWith("file://"))
					{
						str0 = str0.substring(7);
					}
					
					org.gjt.sp.jedit.io.VFSManager.runInWorkThread(new DraggedURLLoader(textArea,str0));
				}
				found = true;
				
			}
			
			if (found) {
				return true;
			}
		}

		if(dragSource != null
			&& textArea.getBuffer()
			== dragSource.getBuffer())
		{
			compoundEdit = true;
			textArea.getBuffer().beginCompoundEdit();
		}
		
		
		sameTextArea = (textArea == dragSource);

		int caret = textArea.getCaretPosition();
		Selection s = textArea.getSelectionAtOffset(caret);

		
		if(s != null)
		{
			if(sameTextArea)
				return false;
			
			int startPos = s.start;
			textArea.setSelectedText(s,str);
			textArea.setSelection(new Selection.Range(startPos,startPos+str.length()));
		}
		
		else
		{
			if (sameTextArea)
			{
				insertPos = caret;
				insertOffset = 0;
				Selection[] selections = textArea.getSelection();
				for (int i=0;i<selections.length;i++)
				{
					if (selections[i].end<(insertPos+insertOffset))
						insertOffset-=(selections[i].end-selections[i].start);
				}
			}
			else
			{
				textArea.getBuffer().insert(caret,str);
				textArea.setSelection(new Selection.Range(caret,caret+str.length()));
			}
		}
		textArea.scrollToCaret(true);

		return true;
	} 

	
	protected void exportDone(JComponent c, Transferable t,
		int action)
	{
		Log.log(Log.DEBUG,this,"Export done");

		JEditTextArea textArea = (JEditTextArea)c;

		try
		{
			
			if (action == NONE)
			{
				Log.log(Log.DEBUG,this,"Export impossible");
				return;
			}

			if(t == null)
			{
				Log.log(Log.DEBUG,this,"=> Null transferrable");
				textArea.selectNone();
			}
			else if(t.isDataFlavorSupported(
				DataFlavor.stringFlavor))
			{
				Log.log(Log.DEBUG,this,"=> String");
				if (sameTextArea)
				{
					if(action == MOVE)
					{
						textArea.setSelectedText(null,false);
						insertPos += insertOffset;
					} 
					try
					{
						String str = (String)t.getTransferData(DataFlavor.stringFlavor);
						textArea.getBuffer().insert(insertPos,str);
						textArea.setSelection(new Selection.Range(insertPos,insertPos+str.length()));
					}
					catch(Exception e)
					{
						Log.log(Log.DEBUG,this,"exportDone in sameTextArea");
						Log.log(Log.DEBUG,this,e);
					}
				}
				else
				{
					if(action == MOVE)
						textArea.setSelectedText(null,false);
					else
						textArea.selectNone();
				}
			}
		}
		finally
		{
			if(compoundEdit)
			{
				compoundEdit = false;
				textArea.getBuffer().endCompoundEdit();
			}
		}

		dragSource = null;
	} 

	
	public boolean canImport(JComponent c, DataFlavor[] flavors)
	{
		JEditTextArea textArea = (JEditTextArea)c;

		
		
		boolean returnValue = false;

		for(int i = 0; i < flavors.length; i++)
		{
			if(flavors[i].equals(
				DataFlavor.javaFileListFlavor))
			{
				returnValue = true;
			}
			else if(flavors[i].equals(
				DataFlavor.stringFlavor))
			{
				if(textArea.isEditable())
					returnValue = true;
			}
		}

		Log.log(Log.DEBUG,this,"canImport() returning "
			+ returnValue);
		return returnValue;
	} 

	
	static class TextAreaSelection extends StringSelection
	{
		JEditTextArea textArea;

		TextAreaSelection(JEditTextArea textArea)
		{
			super(textArea.getSelectedText());
			this.textArea = textArea;
		}
	} 

	
	class DraggedURLLoader extends org.gjt.sp.util.WorkRequest
	{
		private JEditTextArea textArea;
		private String url;
		
		public DraggedURLLoader(JEditTextArea textArea, String url)
		{
			super();
			this.textArea = textArea;
			this.url = url;
		}
		public void run()
		{
			jEdit.openFile(textArea.getView(),url);
		}
	} 

}
