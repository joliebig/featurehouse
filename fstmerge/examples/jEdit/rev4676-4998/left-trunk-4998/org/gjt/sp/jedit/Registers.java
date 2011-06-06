

package org.gjt.sp.jedit;


import com.microstar.xml.*;
import java.awt.datatransfer.*;
import java.awt.Toolkit;
import java.io.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.util.Log;



public class Registers
{
	
	
	public static void copy(JEditTextArea textArea, char register)
	{
		String selection = textArea.getSelectedText();
		if(selection == null)
			return;

		setRegister(register,selection);
		HistoryModel.getModel("clipboard").addItem(selection);
	} 

	
	
	public static void cut(JEditTextArea textArea, char register)
	{
		if(textArea.isEditable())
		{
			String selection = textArea.getSelectedText();
			if(selection == null)
				return;

			setRegister(register,selection);
			HistoryModel.getModel("clipboard").addItem(selection);

			textArea.setSelectedText("");
		}
		else
			textArea.getToolkit().beep();
	} 

	
	
	public static void append(JEditTextArea textArea, char register)
	{
		append(textArea,register,"\n",false);
	} 

	
	
	public static void append(JEditTextArea textArea, char register,
		String separator)
	{
		append(textArea,register,separator,false);
	} 

	
	
	public static void append(JEditTextArea textArea, char register,
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
			String registerContents = reg.toString();
			if(registerContents != null)
			{
				if(registerContents.endsWith(separator))
					selection = registerContents + selection;
				else
					selection = registerContents + separator + selection;
			}
		}

		setRegister(register,selection);
		HistoryModel.getModel("clipboard").addItem(selection);

		if(cut)
			textArea.setSelectedText("");
	} 

	
	
	public static void paste(JEditTextArea textArea, char register)
	{
		paste(textArea,register,false);
	} 

	
	
	public static void paste(JEditTextArea textArea, char register,
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
		else
		{
			String selection = reg.toString();
			if(selection == null)
			{
				textArea.getToolkit().beep();
				return;
			}

			if(vertical && textArea.getSelectionCount() == 0)
			{
				Buffer buffer = textArea.getBuffer();

				try
				{
					buffer.beginCompoundEdit();

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
				finally
				{
					buffer.endCompoundEdit();
				}
			}
			else
				textArea.setSelectedText(selection);

			HistoryModel.getModel("clipboard").addItem(selection);
		}
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
		if(name != '%' && name != '$')
		{
			if(!loaded)
				loadRegisters();

			if(!loading)
				modified = true;
		}

		if(name >= registers.length)
		{
			Register[] newRegisters = new Register[
				Math.min(1<<16,name * 2)];
			System.arraycopy(registers,0,newRegisters,0,
				registers.length);
			registers = newRegisters;
		}

		registers[name] = newRegister;
	} 

	
	
	public static void setRegister(char name, String value)
	{
		Register register = getRegister(name);
		if(register != null)
			register.setValue(value);
		else
			setRegister(name,new StringRegister(value));
	} 

	
	
	public static void clearRegister(char name)
	{
		if(name >= registers.length)
			return;

		Register register = registers[name];
		if(name == '$' || name == '%')
			register.setValue("");
		else
			registers[name] = null;
	} 

	
	
	public static Register[] getRegisters()
	{
		if(!loaded)
			loadRegisters();
		return registers;
	} 

	
	
	public static String getRegisterStatusPrompt(String action)
	{
		return jEdit.getProperty("view.status." + action,
			new String[] { getRegisterNameString() });
	} 

	
	
	public static String getRegisterNameString()
	{
		if(!loaded)
			loadRegisters();

		StringBuffer buf = new StringBuffer();
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
			return jEdit.getProperty("view.status.no-registers");
		else
			return buf.toString();
	} 

	
	public static void saveRegisters()
	{
		if(!loaded || !modified)
			return;

		Log.log(Log.MESSAGE,Registers.class,"Saving registers.xml");
		File file1 = new File(MiscUtilities.constructPath(
			jEdit.getSettingsDirectory(), "#registers.xml#save#"));
		File file2 = new File(MiscUtilities.constructPath(
			jEdit.getSettingsDirectory(), "registers.xml"));
		if(file2.exists() && file2.lastModified() != registersModTime)
		{
			Log.log(Log.WARNING,Registers.class,file2 + " changed"
				+ " on disk; will not save registers");
			return;
		}

		jEdit.backupSettingsFile(file2);

		String lineSep = System.getProperty("line.separator");

		try
		{
			BufferedWriter out = new BufferedWriter(
				new FileWriter(file1));

			out.write("<?xml version=\"1.0\"?>");
			out.write(lineSep);
			out.write("<!DOCTYPE REGISTERS SYSTEM \"registers.dtd\">");
			out.write(lineSep);
			out.write("<REGISTERS>");
			out.write(lineSep);

			Register[] registers = getRegisters();
			for(int i = 0; i < registers.length; i++)
			{
				Register register = registers[i];
				if(register == null || i == '$' || i == '%')
					continue;

				out.write("<REGISTER NAME=\"");
				if(i == '"')
					out.write("&quot;");
				else
					out.write((char)i);
				out.write("\">");

				out.write(MiscUtilities.charsToEntities(
					register.toString()));

				out.write("</REGISTER>");
				out.write(lineSep);
			}

			out.write("</REGISTERS>");
			out.write(lineSep);

			out.close();

			
			file2.delete();
			file1.renameTo(file2);
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,Registers.class,e);
		}

		registersModTime = file2.lastModified();
		modified = false;
	} 

	
	private static Register[] registers;
	private static long registersModTime;
	private static boolean loaded, loading, modified;

	private Registers() {}

	static
	{
		registers = new Register[256];
		registers['$'] = new ClipboardRegister(Toolkit
			.getDefaultToolkit().getSystemClipboard());
	}

	
	private static void loadRegisters()
	{
		String settingsDirectory = jEdit.getSettingsDirectory();
		if(settingsDirectory == null)
			return;

		File registerFile = new File(MiscUtilities.constructPath(
			jEdit.getSettingsDirectory(),"registers.xml"));
		if(!registerFile.exists())
			return;

		registersModTime = registerFile.lastModified();
		loaded = true;

		Log.log(Log.MESSAGE,jEdit.class,"Loading registers.xml");

		RegistersHandler handler = new RegistersHandler();
		XmlParser parser = new XmlParser();
		parser.setHandler(handler);
		Reader in = null;
		try
		{
			loading = true;
			in = new BufferedReader(new FileReader(registerFile));
			parser.parse(null, null, in);
		}
		catch(XmlException xe)
		{
			int line = xe.getLine();
			String message = xe.getMessage();
			Log.log(Log.ERROR,Registers.class,registerFile + ":"
				+ line + ": " + message);
		}
		catch(FileNotFoundException fnf)
		{
			
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,Registers.class,e);
		}
		finally
		{
			loading = false;
			try
			{
				if(in != null)
					in.close();
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,Registers.class,io);
			}
		}
	} 

	

	

	
	
	public interface Register
	{
		
		String toString();

		
		void setValue(String value);
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

		
		public String toString()
		{
			try
			{
				String selection = (String)(clipboard
					.getContents(this).getTransferData(
					DataFlavor.stringFlavor));

				boolean trailingEOL = (selection.endsWith("\n")
					|| selection.endsWith(System.getProperty(
					"line.separator")));

				
				
				
				BufferedReader in = new BufferedReader(
					new StringReader(selection));
				StringBuffer buf = new StringBuffer();
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
	} 

	
	
	public static class StringRegister implements Register
	{
		private String value;

		
		public StringRegister(String value)
		{
			this.value = value;
		}

		
		public void setValue(String value)
		{
			this.value = value;
		}

		
		public String toString()
		{
			return value;
		}

		
		public void dispose() {}
	} 

	
	static class RegistersHandler extends HandlerBase
	{
		
		public Object resolveEntity(String publicId, String systemId)
		{
			if("registers.dtd".equals(systemId))
			{
				
				
				
				return new StringReader("<!-- -->");

				
			}

			return null;
		} 

		
		public void attribute(String aname, String value, boolean isSpecified)
		{
			if(aname.equals("NAME"))
				registerName = value;
		} 

		
		public void doctypeDecl(String name, String publicId,
			String systemId) throws Exception
		{
			if("REGISTERS".equals(name))
				return;

			Log.log(Log.ERROR,this,"registers.xml: DOCTYPE must be REGISTERS");
		} 

		
		public void endElement(String name)
		{
			if(name.equals("REGISTER"))
			{
				if(registerName == null || registerName.length() != 1)
					Log.log(Log.ERROR,this,"Malformed NAME: " + registerName);
				else
					setRegister(registerName.charAt(0),charData);
			}
		} 

		
		public void charData(char[] ch, int start, int length)
		{
			charData = new String(ch,start,length);
		} 

		
		private String registerName;
		private String charData;
		
	} 

	
}
