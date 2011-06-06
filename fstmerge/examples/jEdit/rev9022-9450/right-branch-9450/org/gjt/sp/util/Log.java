

package org.gjt.sp.util;

import java.io.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;


public class Log
{
	
	
	public static final int MAXLINES = 500;

	
	public static final int DEBUG = 1;

	
	public static final int MESSAGE = 3;

	
	public static final int NOTICE = 5;

	
	public static final int WARNING = 7;

	
	public static final int ERROR = 9;
	

	
	
	public static void init(boolean stdio, int level)
	{
		if(stdio)
		{
			if(System.out == realOut && System.err == realErr)
			{
				System.setOut(createPrintStream(NOTICE,null));
				System.setErr(createPrintStream(ERROR,null));
			}
		}

		Log.level = level;

		
		log(MESSAGE,Log.class,"When reporting bugs, please"
			+ " include the following information:");
		String[] props = {
			"java.version", "java.vm.version", "java.runtime.version",
			"java.vendor", "java.compiler", "os.name", "os.version",
			"os.arch", "user.home", "java.home",
			"java.class.path",
			};
		for(int i = 0; i < props.length; i++)
		{
			log(MESSAGE,Log.class,
				props[i] + '=' + System.getProperty(props[i]));
		}
	} 

	
	
	public static void setLogWriter(Writer stream)
	{
		if(Log.stream == null && stream != null)
		{
			try
			{
				if(wrap)
				{
					for(int i = logLineCount; i < log.length; i++)
					{
						stream.write(log[i]);
						stream.write(lineSep);
					}
				}
				for(int i = 0; i < logLineCount; i++)
				{
					stream.write(log[i]);
					stream.write(lineSep);
				}

				stream.flush();
			}
			catch(Exception e)
			{
				
			}
		}

		Log.stream = stream;
	} 

	
	
	public static void flushStream()
	{
		if(stream != null)
		{
			try
			{
				stream.flush();
			}
			catch(IOException io)
			{
				io.printStackTrace(realErr);
			}
		}
	} 

	
	
	public static void closeStream()
	{
		if(stream != null)
		{
			try
			{
				stream.close();
				stream = null;
			}
			catch(IOException io)
			{
				io.printStackTrace(realErr);
			}
		}
	} 

	
	
	public static ListModel getLogListModel()
	{
		return listModel;
	} 

	
	
	public static void log(int urgency, Object source, Object message,
		Throwable exception)
	{
		
		log(urgency,source,message);
		log(urgency,source,exception);
	} 

	
	
	public static void log(int urgency, Object source, Object message)
	{
		String _source;
		if(source == null)
		{
			_source = Thread.currentThread().getName();
			if(_source == null)
			{
				_source = Thread.currentThread().getClass().getName();
			}
		}
		else if(source instanceof Class)
			_source = ((Class)source).getName();
		else
			_source = source.getClass().getName();
		int index = _source.lastIndexOf('.');
		if(index != -1)
			_source = _source.substring(index+1);

		if(message instanceof Throwable)
		{
			_logException(urgency,source,(Throwable)message);
		}
		else
		{
			String _message = String.valueOf(message);
			
			
			synchronized(LOCK)
			{
				StringTokenizer st = new StringTokenizer(
					_message,"\r\n");
				int lineCount = 0;
				boolean oldWrap = wrap;
				while(st.hasMoreTokens())
				{
					lineCount++;
					_log(urgency,_source,st.nextToken()
						.replace('\t',' '));
				}
				listModel.update(lineCount,oldWrap);
			}
		}
	} 

	

	
	private static final Object LOCK = new Object();
	private static final String[] log;
	private static int logLineCount;
	private static boolean wrap;
	private static int level = WARNING;
	private static Writer stream;
	private static final String lineSep;
	private static final PrintStream realOut;
	private static final PrintStream realErr;
	private static final LogListModel listModel;
	

	
	static
	{
		level = WARNING;

		realOut = System.out;
		realErr = System.err;

		log = new String[MAXLINES];
		lineSep = System.getProperty("line.separator");
		listModel = new LogListModel();
	} 

	
	private static PrintStream createPrintStream(final int urgency,
		final Object source)
	{
		return new LogPrintStream(urgency, source);
	} 

	
	private static void _logException(final int urgency,
		final Object source,
		final Throwable message)
	{
		PrintStream out = createPrintStream(urgency,source);

		synchronized(LOCK)
		{
			message.printStackTrace(out);
		}
	} 

	
	private static void _log(int urgency, String source, String message)
	{
		String fullMessage = '[' + urgencyToString(urgency) + "] " + source
			+ ": " + message;

		try
		{
			log[logLineCount] = fullMessage;
			if(++logLineCount >= log.length)
			{
				wrap = true;
				logLineCount = 0;
			}

			if(stream != null)
			{
				stream.write(fullMessage);
				stream.write(lineSep);
			}
		}
		catch(Exception e)
		{
			e.printStackTrace(realErr);
		}

		if(urgency >= level)
		{
			if(urgency == ERROR)
				realErr.println(fullMessage);
			else
				realOut.println(fullMessage);
		}
	} 

	
	private static String urgencyToString(int urgency)
	{
		switch(urgency)
		{
		case DEBUG:
			return "debug";
		case MESSAGE:
			return "message";
		case NOTICE:
			return "notice";
		case WARNING:
			return "warning";
		case ERROR:
			return "error";
		}

		throw new IllegalArgumentException("Invalid urgency: " + urgency);
	} 

	

	
	static class LogListModel implements ListModel
	{
		final List<ListDataListener> listeners = new ArrayList<ListDataListener>();

		private void fireIntervalAdded(int index1, int index2)
		{
			for(int i = 0; i < listeners.size(); i++)
			{
				ListDataListener listener = listeners.get(i);
				listener.intervalAdded(new ListDataEvent(this,
					ListDataEvent.INTERVAL_ADDED,
					index1,index2));
			}
		}

		private void fireIntervalRemoved(int index1, int index2)
		{
			for(int i = 0; i < listeners.size(); i++)
			{
				ListDataListener listener = listeners.get(i);
				listener.intervalRemoved(new ListDataEvent(this,
					ListDataEvent.INTERVAL_REMOVED,
					index1,index2));
			}
		}

		public void addListDataListener(ListDataListener listener)
		{
			listeners.add(listener);
		}

		public void removeListDataListener(ListDataListener listener)
		{
			listeners.remove(listener);
		}

		public Object getElementAt(int index)
		{
			if(wrap)
			{
				if(index < MAXLINES - logLineCount)
					return log[index + logLineCount];
				else
					return log[index - MAXLINES + logLineCount];
			}
			else
				return log[index];
		}

		public int getSize()
		{
			if(wrap)
				return MAXLINES;
			else
				return logLineCount;
		}

		void update(final int lineCount, final boolean oldWrap)
		{
			if(lineCount == 0 || listeners.isEmpty())
				return;

			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					if(wrap)
					{
						if(oldWrap)
							fireIntervalRemoved(0,lineCount - 1);
						else
						{
							fireIntervalRemoved(0,
								logLineCount);
						}
						fireIntervalAdded(
							MAXLINES - lineCount + 1,
							MAXLINES);
					}
					else
					{
						fireIntervalAdded(
							logLineCount - lineCount + 1,
							logLineCount);
					}
				}
			});
		}
	} 

	
	private static class LogPrintStream extends PrintStream {

		private final ByteArrayOutputStream buffer;
		private final OutputStream orig;

		LogPrintStream(int urgency, Object source)
		{
			super(new LogOutputStream(urgency, source));
			buffer = new ByteArrayOutputStream();
			orig = out;
		}

		
		public PrintStream printf(String format, Object... args)
		{
			synchronized (orig)
			{
				buffer.reset();
				out = buffer;
				super.printf(format, args);

				try
				{
					byte[] data = buffer.toByteArray();
					orig.write(data, 0, data.length);
					out = orig;
				}
				catch (IOException ioe)
				{
					
				}
				finally
				{
					buffer.reset();
				}
			}
			return this;
		}
	}

	private static class LogOutputStream extends OutputStream
	{
		private final int 	urgency;
		private final Object 	source;

		LogOutputStream(int urgency, Object source)
		{
			this.urgency 	= urgency;
			this.source 	= source;
		}

		public synchronized void write(int b)
		{
			byte[] barray = { (byte)b };
			write(barray,0,1);
		}

		public synchronized void write(byte[] b, int off, int len)
		{
			String str = new String(b,off,len);
			log(urgency,source,str);
		}
	}

}

