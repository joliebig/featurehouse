

package org.gjt.sp.jedit;


import java.awt.datatransfer.*;
import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.io.*;

import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.datatransfer.JEditRichText;
import org.gjt.sp.jedit.datatransfer.RichTextTransferable;
import org.gjt.sp.jedit.datatransfer.TransferHandler;
import org.gjt.sp.jedit.gui.HistoryModel;
import org.gjt.sp.jedit.textarea.TextArea;
import org.gjt.sp.jedit.textarea.Selection;
import org.gjt.sp.util.Log;



public class Registers
{
	
	
	public static void copy(TextArea textArea, char register)
	{
		String selection = textArea.getSelectedText();
		if(selection == null)
			return;

		Transferable transferable = TransferHandler.getInstance().getTransferable(textArea, selection);
		setRegister(register, transferable);
		HistoryModel.getModel("clipboard").addItem(selection);

	} 

	
	
	public static void cut(TextArea textArea, char register)
	{
		if(textArea.isEditable())
		{
			String selection = textArea.getSelectedText();
			if(selection == null)
				return;

			Transferable transferable = TransferHandler.getInstance().getTransferable(textArea, selection);
			setRegister(register,transferable);
			HistoryModel.getModel("clipboard").addItem(selection);

			textArea.setSelectedText("");
		}
		else
			textArea.getToolkit().beep();
	} 

	
	
	public static void append(TextArea textArea, char register)
	{
		append(textArea,register,"\n",false);
	} 

	
	
	public static void append(TextArea textArea, char register,
		String separator)
	{
		append(textArea,register,separator,false);
	} 

	
	
	public static void append(TextArea textArea, char register,
		String separator, boolean cut)
	{
		if(cut && !textArea.isEditable())
		{
			textArea.getToolkit().beep();
			return;
		}

		String selection = textArea.getSelectedText();
		if(selection == null)
			return;

		Register reg = getRegister(register);

		if(reg != null)
		{
			Transferable transferable = reg.getTransferable();
			if (transferable.isDataFlavorSupported(DataFlavor.stringFlavor))
			{
				try
				{
					String registerContents = (String) transferable.getTransferData(DataFlavor.stringFlavor);
					if(registerContents != null)
					{
						if(registerContents.endsWith(separator))
							selection = registerContents + selection;
						else
							selection = registerContents + separator + selection;
					}
				}
				catch (UnsupportedFlavorException e)
				{
				}
				catch (IOException e)
				{
					Log.log(Log.ERROR, Registers.class, e);
				}
			}
		}
		Transferable transferable = TransferHandler.getInstance().getTransferable(textArea, selection);
		setRegister(register,transferable);
		HistoryModel.getModel("clipboard").addItem(selection);

		if(cut)
			textArea.setSelectedText("");
	} 

	
	
	public static void paste(TextArea textArea, char register)
	{
		paste(textArea,register,false);
	}

	
	public static void paste(TextArea textArea, char register,
		boolean vertical)
	{
		if(!textArea.isEditable())
		{
			textArea.getToolkit().beep();
			return;
		}

		Register reg = getRegister(register);

		if(reg == null)
		{
			textArea.getToolkit().beep();
			return;
		}
		Transferable transferable = reg.getTransferable();
		Mode mode = null;
		String selection = null;
		if (transferable.isDataFlavorSupported(RichTextTransferable.jEditRichTextDataFlavor))
		{
			try
			{
				JEditRichText data = (JEditRichText) transferable.getTransferData(RichTextTransferable.jEditRichTextDataFlavor);
				mode = data.getMode();
				selection = data.getText();
			}
			catch (UnsupportedFlavorException e)
			{
				e.printStackTrace();
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
		}
		else if (transferable.isDataFlavorSupported(DataFlavor.stringFlavor))
		{
			try
			{
				selection = (String) transferable.getTransferData(DataFlavor.stringFlavor);
			}
			catch (UnsupportedFlavorException e)
			{
				e.printStackTrace();
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
		}
		if(selection == null)
		{
			textArea.getToolkit().beep();
			return;
		}
		JEditBuffer buffer = textArea.getBuffer();
		try
		{
			buffer.beginCompoundEdit();

			
			if(vertical && textArea.getSelectionCount() == 0)
			{
				int caret = textArea.getCaretPosition();
				int caretLine = textArea.getCaretLine();
				Selection.Rect rect = new Selection.Rect(
					caretLine,caret,caretLine,caret);
				textArea.setSelectedText(rect,selection);
				caretLine = textArea.getCaretLine();

				if(caretLine != textArea.getLineCount() - 1)
				{

					int startColumn = rect.getStartColumn(
						buffer);
					int offset = buffer
						.getOffsetOfVirtualColumn(
						caretLine + 1,startColumn,null);
					if(offset == -1)
					{
						buffer.insertAtColumn(caretLine + 1,startColumn,"");
						textArea.setCaretPosition(
							buffer.getLineEndOffset(
							caretLine + 1) - 1);
					}
					else
					{
						textArea.setCaretPosition(
							buffer.getLineStartOffset(
							caretLine + 1) + offset);
					}
				}
			}
			else 
			{
				textArea.replaceSelection(selection);
			}
		}
		finally
		{
			buffer.endCompoundEdit();
		}
		if (mode != null &&
			"text".equals(buffer.getMode().getName()) &&
			!mode.equals(buffer.getMode()))
		{
			buffer.setMode(mode);
		}
		HistoryModel.getModel("clipboard").addItem(selection);
	} 

	
	
	public static Register getRegister(char name)
	{
		if(name != '$' && name != '%')
		{
			if(!loaded)
				loadRegisters();
		}

		if(registers == null || name >= registers.length)
			return null;
		else
			return registers[name];
	} 

	
	
	public static void setRegister(char name, Register newRegister)
	{
		touchRegister(name);

		if(name >= registers.length)
		{
			Register[] newRegisters = new Register[
				Math.min(1<<16, name<<1)];
			System.arraycopy(registers,0,newRegisters,0,
				registers.length);
			registers = newRegisters;
		}

		registers[name] = newRegister;
		if (listener != null)
			listener.registerChanged(name);
	} 

	
	
	public static void setRegister(char name, String value)
	{
		setRegister(name, new StringSelection(value));
	} 

	
	
	public static void setRegister(char name, Transferable transferable)
	{
		touchRegister(name);
		Register register = getRegister(name);
		if(register != null)
		{
			register.setTransferable(transferable);
			if (listener != null)
				listener.registerChanged(name);
		}
		else
		{
			DefaultRegister defaultRegister = new DefaultRegister();
			defaultRegister.setTransferable(transferable);
			setRegister(name, defaultRegister);
		}
	} 

	
	
	public static void clearRegister(char name)
	{
		if(name >= registers.length)
			return;

		Register register = registers[name];
		if(name == '$' || name == '%')
			register.setValue("");
		else
		{
			registers[name] = null;
			modified = true;
			if (listener != null)
				listener.registerChanged(name);
		}
	} 

	
	
	public static Register[] getRegisters()
	{
		if(!loaded)
			loadRegisters();
		return registers;
	} 

	
	
	public static String getRegisterNameString()
	{
		if(!loaded)
			loadRegisters();

		StringBuilder buf = new StringBuilder(registers.length << 1);
		for(int i = 0; i < registers.length; i++)
		{
			if(registers[i] != null)
			{
				if(buf.length() != 0)
					buf.append(' ');
				buf.append((char)i);
			}
		}

		if(buf.length() == 0)
			return null;
		else
			return buf.toString();
	} 

	
	public static void saveRegisters()
	{
		if(!loaded || !modified)
			return;

		if (saver != null)
		{
			saver.saveRegisters();
			modified = false;
		}
	} 

	
	public static void setListener(RegistersListener listener)
	{
		Registers.listener = listener;
	} 

	
	public static void setSaver(RegisterSaver saver)
	{
		Registers.saver = saver;
	} 

	
	public static boolean isLoading()
	{
		return loading;
	} 

	
	public static void setLoading(boolean loading)
	{
		Registers.loading = loading;
	} 

	
	private static Register[] registers;
	private static boolean loaded, loading;
	private static RegisterSaver saver;
	private static RegistersListener listener;
	
	private static boolean modified;

	private Registers() {}

	static
	{
		registers = new Register[256];
		Toolkit toolkit = Toolkit.getDefaultToolkit();
		registers['$'] = new ClipboardRegister(
			toolkit.getSystemClipboard());
		Clipboard selection = toolkit.getSystemSelection();
		if(selection != null)
			registers['%'] = new ClipboardRegister(selection);
	}

	
	private static void touchRegister(char name)
	{
		if(name == '%' || name == '$')
			return;

		if(!loaded)
			loadRegisters();

		if(!loading)
			modified = true;
	} 

	
	private static void loadRegisters()
	{
		if (saver != null)
		{
			loaded = true;
			saver.loadRegisters();
		}
	} 

	

	

	
	
	public interface Register
	{
		
		@Deprecated
		String toString();

		
		@Deprecated
		void setValue(String value);

		Transferable getTransferable();

		void setTransferable(Transferable transferable);
	} 

	
	
	public static class ClipboardRegister implements Register
	{
		Clipboard clipboard;

		public ClipboardRegister(Clipboard clipboard)
		{
			this.clipboard = clipboard;
		}

		
		public void setValue(String value)
		{
			StringSelection selection = new StringSelection(value);
			clipboard.setContents(selection,null);
		}

		
		@Override
		public String toString()
		{
			try
			{

				if (false)
				{
					
					Log.log(Log.DEBUG,this,"clipboard.getContents(this)="+clipboard.getContents(this)+'.');
					debugListDataFlavors(clipboard.getContents(this));
				}

				String selection = (String)clipboard
					.getContents(this).getTransferData(
					DataFlavor.stringFlavor);

				boolean trailingEOL = selection.endsWith("\n")
					|| selection.endsWith(System.getProperty(
					"line.separator"));

				
				
				
				BufferedReader in = new BufferedReader(
					new StringReader(selection));
				StringBuilder buf = new StringBuilder();
				String line;
				while((line = in.readLine()) != null)
				{
					
					
					if(line.endsWith("\0"))
					{
						line = line.substring(0,
							line.length() - 1);
					}
					buf.append(line);
					buf.append('\n');
				}
				
				if(!trailingEOL && buf.length() != 0)
					buf.setLength(buf.length() - 1);
				return buf.toString();
			}
			catch(Exception e)
			{
				Log.log(Log.NOTICE,this,e);
				return null;
			}
		}

		public Transferable getTransferable()
		{
			return clipboard.getContents(this);
		}

		public void setTransferable(Transferable transferable)
		{
			clipboard.setContents(transferable, null);
		}
	} 

	
	protected static void debugListDataFlavors(Transferable transferable)
	{
		DataFlavor[] dataFlavors = transferable.getTransferDataFlavors();

		for (int i = 0;i<dataFlavors.length;i++)
		{
			DataFlavor dataFlavor = dataFlavors[i];
			Log.log(Log.DEBUG,Registers.class,
				"debugListDataFlavors(): dataFlavor="+
				dataFlavor+'.');
		}

		if (dataFlavors.length == 0)
		{
			Log.log(Log.DEBUG,Registers.class,
				"debugListDataFlavors(): no dataFlavor supported.");
		}
	} 

	
	private static class DefaultRegister implements Register
	{
		private Transferable transferable;

		public void setValue(String value)
		{
			this.transferable = new StringSelection(value);
		}

		@Override
		public String toString()
		{
			if (transferable == null)
				return null;
			if (transferable.isDataFlavorSupported(DataFlavor.stringFlavor))
			{
				try
				{
					return transferable.getTransferData(DataFlavor.stringFlavor).toString();
				}
				catch (UnsupportedFlavorException e)
				{
					Log.log(Log.ERROR, this, e);
				}
				catch (IOException e)
				{
					Log.log(Log.ERROR, this, e);
				}
			}
			return transferable.toString();
		}

		public Transferable getTransferable()
		{
			return transferable;
		}

		public void setTransferable(Transferable transferable)
		{
			this.transferable = transferable;
		}
	} 

	
	
	@Deprecated
	public static class StringRegister extends DefaultRegister
	{
		
		public StringRegister(String value)
		{
			setValue(value);
		}
	} 

	
}
