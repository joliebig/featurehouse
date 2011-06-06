

package org.gjt.sp.jedit.gui;


import javax.swing.border.*;
import javax.swing.text.Segment;
import javax.swing.*;
import java.awt.event.*;
import java.awt.font.*;
import java.awt.geom.*;
import java.awt.*;
import java.text.*;
import java.util.Calendar;
import java.util.Date;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.*;



public class StatusBar extends JPanel implements WorkThreadProgressListener
{
	
	public StatusBar(View view)
	{
		super(new BorderLayout());
		setBorder(new CompoundBorder(new EmptyBorder(4,0,0,
			(OperatingSystem.isMacOS() ? 18 : 0)),
			UIManager.getBorder("TextField.border")));

		this.view = view;

		panel = new JPanel(new BorderLayout());
		box = new Box(BoxLayout.X_AXIS);
		panel.add(BorderLayout.EAST,box);
		add(BorderLayout.CENTER,panel);

		MouseHandler mouseHandler = new MouseHandler();

		caretStatus = new ToolTipLabel();
		caretStatus.setToolTipText(jEdit.getProperty("view.status.caret-tooltip"));
		caretStatus.addMouseListener(mouseHandler);

		message = new JLabel(" ");
		setMessageComponent(message);

		mode = new ToolTipLabel();
		mode.setToolTipText(jEdit.getProperty("view.status.mode-tooltip"));
		mode.addMouseListener(mouseHandler);

		wrap = new ToolTipLabel();
		wrap.setHorizontalAlignment(SwingConstants.CENTER);
		wrap.setToolTipText(jEdit.getProperty("view.status.wrap-tooltip"));
		wrap.addMouseListener(mouseHandler);

		multiSelect = new ToolTipLabel();
		multiSelect.setHorizontalAlignment(SwingConstants.CENTER);
		multiSelect.setToolTipText(jEdit.getProperty("view.status.multi-tooltip"));
		multiSelect.addMouseListener(mouseHandler);

		rectSelect = new ToolTipLabel();
		rectSelect.setHorizontalAlignment(SwingConstants.CENTER);
		rectSelect.setToolTipText(jEdit.getProperty("view.status.rect-tooltip"));
		rectSelect.addMouseListener(mouseHandler);

		overwrite = new ToolTipLabel();
		overwrite.setHorizontalAlignment(SwingConstants.CENTER);
		overwrite.setToolTipText(jEdit.getProperty("view.status.overwrite-tooltip"));
		overwrite.addMouseListener(mouseHandler);

		lineSep = new ToolTipLabel();
		lineSep.setHorizontalAlignment(SwingConstants.CENTER);
		lineSep.setToolTipText(jEdit.getProperty("view.status.linesep-tooltip"));
		lineSep.addMouseListener(mouseHandler);
	} 

	
	public void propertiesChanged()
	{
		Color fg = jEdit.getColorProperty("view.status.foreground");
		Color bg = jEdit.getColorProperty("view.status.background");

		showCaretStatus = jEdit.getBooleanProperty("view.status.show-caret-status");
		showEditMode = jEdit.getBooleanProperty("view.status.show-edit-mode");
		showFoldMode = jEdit.getBooleanProperty("view.status.show-fold-mode");
		showEncoding = jEdit.getBooleanProperty("view.status.show-encoding");
		showWrap = jEdit.getBooleanProperty("view.status.show-wrap");
		showMultiSelect = jEdit.getBooleanProperty("view.status.show-multi-select");
		showRectSelect = jEdit.getBooleanProperty("view.status.show-rect-select");
		showOverwrite = jEdit.getBooleanProperty("view.status.show-overwrite");
		showLineSeperator = jEdit.getBooleanProperty("view.status.show-line-seperator");
		boolean showMemory = jEdit.getBooleanProperty("view.status.show-memory");
		boolean showClock = jEdit.getBooleanProperty("view.status.show-clock");

		panel.setBackground(bg);
		panel.setForeground(fg);
		caretStatus.setBackground(bg);
		caretStatus.setForeground(fg);
		message.setBackground(bg);
		message.setForeground(fg);
		mode.setBackground(bg);
		mode.setForeground(fg);
		wrap.setBackground(bg);
		wrap.setForeground(fg);
		multiSelect.setBackground(bg);
		multiSelect.setForeground(fg);
		rectSelect.setBackground(bg);
		rectSelect.setForeground(fg);
		overwrite.setBackground(bg);
		overwrite.setForeground(fg);
		lineSep.setBackground(bg);
		lineSep.setForeground(fg);

		
		Font font = new JLabel().getFont();
		
		FontMetrics fm = getFontMetrics(font);
		Dimension dim = null;

		if (showCaretStatus)
		{
			panel.add(BorderLayout.WEST,caretStatus);

			caretStatus.setFont(font);

			dim = new Dimension(fm.stringWidth(caretTestStr),
				fm.getHeight());
                        caretStatus.setPreferredSize(dim);
		}
		else
			panel.remove(caretStatus);

		box.removeAll();

		if (showEncoding || showEditMode || showFoldMode)
			box.add(mode);

		if (showWrap)
		{
			dim = new Dimension(Math.max(
				Math.max(fm.charWidth('-'),fm.charWidth('H')),
				fm.charWidth('S')) + 1,fm.getHeight());
			wrap.setPreferredSize(dim);
			wrap.setMaximumSize(dim);
			box.add(wrap);
		}

		if (showMultiSelect)
		{
			dim = new Dimension(
				Math.max(fm.charWidth('-'),fm.charWidth('M')) + 1,
				fm.getHeight());
			multiSelect.setPreferredSize(dim);
			multiSelect.setMaximumSize(dim);
			box.add(multiSelect);
		}

		if (showRectSelect)
		{
			dim = new Dimension(
				Math.max(fm.charWidth('-'),fm.charWidth('R')) + 1,
				fm.getHeight());
			rectSelect.setPreferredSize(dim);
			rectSelect.setMaximumSize(dim);
			box.add(rectSelect);
		}

		if (showOverwrite)
		{
			dim = new Dimension(
				Math.max(fm.charWidth('-'),fm.charWidth('O')) + 1,
				fm.getHeight());
			overwrite.setPreferredSize(dim);
			overwrite.setMaximumSize(dim);
			box.add(overwrite);
		}

		if (showLineSeperator)
		{
			dim = new Dimension(Math.max(
				Math.max(fm.charWidth('U'),
				fm.charWidth('W')),
				fm.charWidth('M')) + 1,
				fm.getHeight());
			lineSep.setPreferredSize(dim);
			lineSep.setMaximumSize(dim);
			box.add(lineSep);
		}

		if (showMemory)
			box.add(new MemoryStatus());

		if (showClock)
			box.add(new Clock());

		updateBufferStatus();
		updateMiscStatus();
	} 

	
	public void addNotify()
	{
		super.addNotify();
		VFSManager.getIOThreadPool().addProgressListener(this);
	} 

	
	public void removeNotify()
	{
		super.removeNotify();
		VFSManager.getIOThreadPool().removeProgressListener(this);
	} 

	

	
	public void statusUpdate(final WorkThreadPool threadPool, int threadIndex)
	{
		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				
				if(message != null && !"".equals(message.getText().trim())
					&& !currentMessageIsIO)
					return;

				int requestCount = threadPool.getRequestCount();
				if(requestCount == 0)
				{
					setMessageAndClear(jEdit.getProperty(
						"view.status.io.done"));
					currentMessageIsIO = true;
				}
				else if(requestCount == 1)
				{
					setMessage(jEdit.getProperty(
						"view.status.io-1"));
					currentMessageIsIO = true;
				}
				else
				{
					Object[] args = { new Integer(requestCount) };
					setMessage(jEdit.getProperty(
						"view.status.io",args));
					currentMessageIsIO = true;
				}
			}
		});
	} 

	
	public void progressUpdate(WorkThreadPool threadPool, int threadIndex)
	{
	} 

	

	
	
	public void setMessageAndClear(String message)
	{
		setMessage(message);

		tempTimer = new Timer(0,new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				
				if(isShowing())
					setMessage(null);
			}
		});

		tempTimer.setInitialDelay(10000);
		tempTimer.setRepeats(false);
		tempTimer.start();
	} 

	
	
	public void setMessage(String message)
	{
		if(tempTimer != null)
		{
			tempTimer.stop();
			tempTimer = null;
		}

		setMessageComponent(this.message);

		if(message == null)
		{
			InputHandler inputHandler = view.getInputHandler();
			 if(view.getMacroRecorder() != null)
				this.message.setText(jEdit.getProperty("view.status.recording"));
			else
				this.message.setText(" ");
		}
		else
			this.message.setText(message);
	} 

	
	public void setMessageComponent(Component comp)
	{
		currentMessageIsIO = false;

		if (comp == null || messageComp == comp)
		{
			return;
		}

		messageComp = comp;
		panel.add(BorderLayout.CENTER, messageComp);
	} 

	
	public void updateCaretStatus()
	{
		
		

		if (showCaretStatus)
		{
			Buffer buffer = view.getBuffer();

			if(!buffer.isLoaded() ||
				
				buffer != view.getTextArea().getBuffer())
			{
				caretStatus.setText(" ");
				return;
			}

			JEditTextArea textArea = view.getTextArea();

			int currLine = textArea.getCaretLine();

			
			
			
			
			
			if(currLine >= buffer.getLineCount())
				return; 

			int start = textArea.getLineStartOffset(currLine);
			int dot = textArea.getCaretPosition() - start;

			
			if(dot < 0)
				return;

			buffer.getText(start,dot,seg);
			int virtualPosition = MiscUtilities.getVirtualWidth(seg,
				buffer.getTabSize());

			buf.setLength(0);
			buf.append(Integer.toString(currLine + 1));
			buf.append(',');
			buf.append(Integer.toString(dot + 1));

			if (virtualPosition != dot)
			{
				buf.append('-');
				buf.append(Integer.toString(virtualPosition + 1));
			}

			buf.append(' ');

			int firstLine = textArea.getFirstLine();
			int visible = textArea.getVisibleLines();
			int lineCount = textArea.getDisplayManager().getScrollLineCount();

			if (visible >= lineCount)
			{
				buf.append("All");
			}
			else if (firstLine == 0)
			{
				buf.append("Top");
			}
			else if (firstLine + visible >= lineCount)
			{
				buf.append("Bot");
			}
			else
			{
				float percent = (float)firstLine / (float)lineCount
					* 100.0f;
				buf.append(Integer.toString((int)percent));
				buf.append('%');
			}

			caretStatus.setText(buf.toString());
		}
	} 

	
	public void updateBufferStatus()
	{
		
		

		Buffer buffer = view.getBuffer();

		if (showWrap)
		{
			String wrap = buffer.getStringProperty("wrap");
			if(wrap.equals("none"))
				this.wrap.setText("-");
			else if(wrap.equals("hard"))
				this.wrap.setText("H");
			else if(wrap.equals("soft"))
				this.wrap.setText("S");
		}

		if (showLineSeperator)
		{
			String lineSep = buffer.getStringProperty("lineSeparator");
			if("\n".equals(lineSep))
				this.lineSep.setText("U");
			else if("\r\n".equals(lineSep))
				this.lineSep.setText("W");
			else if("\r".equals(lineSep))
				this.lineSep.setText("M");
		}

		if (showEditMode || showFoldMode || showEncoding)
		{
			
			buf.setLength(0);

			if (buffer.isLoaded())
			{
				if (showEditMode)
					buf.append(buffer.getMode().getName());
				if (showFoldMode)
				{
					if (showEditMode)
						buf.append(",");
					buf.append((String)view.getBuffer().getProperty("folding"));
				}
				if (showEncoding)
				{
					if (showEditMode || showFoldMode)
						buf.append(",");
					buf.append(buffer.getStringProperty("encoding"));
				}
			}

			mode.setText("(" + buf.toString() + ")");
		}
	} 

	
	public void updateMiscStatus()
	{
		
		

		JEditTextArea textArea = view.getTextArea();

		if (showMultiSelect)
			multiSelect.setText(textArea.isMultipleSelectionEnabled()
				? "M" : "-");

		if (showRectSelect)
			rectSelect.setText(textArea.isRectangularSelectionEnabled()
				? "R" : "-");

		if (showOverwrite)
			overwrite.setText(textArea.isOverwriteEnabled()
				? "O" : "-");
	} 

	
	private View view;
	private JPanel panel;
	private Box box;
	private ToolTipLabel caretStatus;
	private Component messageComp;
	private JLabel message;
	private JLabel mode;
	private JLabel wrap;
	private JLabel multiSelect;
	private JLabel rectSelect;
	private JLabel overwrite;
	private JLabel lineSep;
	 StringBuffer buf = new StringBuffer();
	private Timer tempTimer;
	private boolean currentMessageIsIO;

	private Segment seg = new Segment();

	private boolean showCaretStatus;
	private boolean showEditMode;
	private boolean showFoldMode;
	private boolean showEncoding;
	private boolean showWrap;
	private boolean showMultiSelect;
	private boolean showRectSelect;
	private boolean showOverwrite;
	private boolean showLineSeperator;
	

	static final String caretTestStr = "9999,999-999 99%";

	
	class MouseHandler extends MouseAdapter
	{
		public void mouseClicked(MouseEvent evt)
		{
			Buffer buffer = view.getBuffer();

			Object source = evt.getSource();
			if(source == caretStatus)
			{
				if(evt.getClickCount() == 2)
					view.getTextArea().showGoToLineDialog();
			}
			else if(source == mode)
			{
				if(evt.getClickCount() == 2)
					new BufferOptions(view,view.getBuffer());
			}
			else if(source == wrap)
				buffer.toggleWordWrap(view);
			else if(source == multiSelect)
				view.getTextArea().toggleMultipleSelectionEnabled();
			else if(source == rectSelect)
				view.getTextArea().toggleRectangularSelectionEnabled();
			else if(source == overwrite)
				view.getTextArea().toggleOverwriteEnabled();
			else if(source == lineSep)
				buffer.toggleLineSeparator(view);
		}
	} 

	
	class ToolTipLabel extends JLabel
	{
		
		public Point getToolTipLocation(MouseEvent event)
		{
			return new Point(event.getX(),-20);
		} 
	} 

	
	class MemoryStatus extends JComponent implements ActionListener
	{
		
		public MemoryStatus()
		{
			
			Font font = new JLabel().getFont();
			
			MemoryStatus.this.setFont(font);

			FontRenderContext frc = new FontRenderContext(
				null,false,false);
			Rectangle2D bounds = font.getStringBounds(		
				memoryTestStr,frc);
			Dimension dim = new Dimension((int)bounds.getWidth(),
				(int)bounds.getHeight());
			setPreferredSize(dim);
			setMaximumSize(dim);
			lm = font.getLineMetrics(memoryTestStr,frc);

			setForeground(jEdit.getColorProperty("view.status.foreground"));
			setBackground(jEdit.getColorProperty("view.status.background"));

			progressForeground = jEdit.getColorProperty(
				"view.status.memory.foreground");
			progressBackground = jEdit.getColorProperty(
				"view.status.memory.background");

			addMouseListener(new MouseHandler());
		} 

		
		public void addNotify()
		{
			super.addNotify();
			timer = new Timer(2000,this);
			timer.start();
			ToolTipManager.sharedInstance().registerComponent(this);
		} 

		
		public void removeNotify()
		{
			timer.stop();
			ToolTipManager.sharedInstance().unregisterComponent(this);
			super.removeNotify();
		} 

		
		public String getToolTipText()
		{
			Runtime runtime = Runtime.getRuntime();
			int freeMemory = (int)(runtime.freeMemory() / 1024);
			int totalMemory = (int)(runtime.totalMemory() / 1024);
			int usedMemory = (totalMemory - freeMemory);
			Integer[] args = { new Integer(usedMemory),
				new Integer(totalMemory) };
			return jEdit.getProperty("view.status.memory-tooltip",args);
		} 

		
		public Point getToolTipLocation(MouseEvent event)
		{
			return new Point(event.getX(),-20);
		} 

		
		public void actionPerformed(ActionEvent evt)
		{
			MemoryStatus.this.repaint();
		} 

		
		public void paintComponent(Graphics g)
		{
			Insets insets = new Insets(0,0,0,0);

			Runtime runtime = Runtime.getRuntime();
			int freeMemory = (int)(runtime.freeMemory() / 1024);
			int totalMemory = (int)(runtime.totalMemory() / 1024);
			int usedMemory = (totalMemory - freeMemory);

			int width = MemoryStatus.this.getWidth()
				- insets.left - insets.right;
			int height = MemoryStatus.this.getHeight()
				- insets.top - insets.bottom - 1;

			float fraction = ((float)usedMemory) / totalMemory;

			g.setColor(progressBackground);

			g.fillRect(insets.left,insets.top,
				(int)(width * fraction),
				height);

			String str = (usedMemory / 1024) + "/"
				+ (totalMemory / 1024) + "Mb";

			FontRenderContext frc = new FontRenderContext(null,false,false);

			Rectangle2D bounds = g.getFont().getStringBounds(str,frc);
		
			Graphics g2 = g.create();
			g2.setClip(insets.left,insets.top,
				(int)(width * fraction),
				height);

			g2.setColor(progressForeground);

			g2.drawString(str,
				insets.left + (int)(width - bounds.getWidth()) / 2,
				(int)(insets.top + lm.getAscent()));

			g2.dispose();

			g2 = g.create();

			g2.setClip(insets.left + (int)(width * fraction),
				insets.top,MemoryStatus.this.getWidth()
				- insets.left - (int)(width * fraction),
				height);

			g2.setColor(MemoryStatus.this.getForeground());

			g2.drawString(str,
				insets.left + (int)(width - bounds.getWidth()) / 2,
				(int)(insets.top + lm.getAscent()));

			g2.dispose();
		} 

		
		private static final String memoryTestStr = "999/999Mb";

		private LineMetrics lm;
		private Color progressForeground;
		private Color progressBackground;

		private Timer timer;
		

		
		class MouseHandler extends MouseAdapter
		{
			public void mousePressed(MouseEvent evt)
			{
				if(evt.getClickCount() == 2)
				{
					jEdit.showMemoryDialog(view);
					repaint();
				}
			}
		} 
	} 

	
	class Clock extends JLabel implements ActionListener
	{
		
		public Clock()
		{
			

			setForeground(jEdit.getColorProperty("view.status.foreground"));
			setBackground(jEdit.getColorProperty("view.status.background"));
		} 

		
		public void addNotify()
		{
			super.addNotify();
			update();

			int millisecondsPerMinute = 1000 * 60;

			timer = new Timer(millisecondsPerMinute,this);
			timer.setInitialDelay((int)(
				millisecondsPerMinute
				- System.currentTimeMillis()
				% millisecondsPerMinute) + 500);
			timer.start();
			ToolTipManager.sharedInstance().registerComponent(this);
		} 

		
		public void removeNotify()
		{
			timer.stop();
			ToolTipManager.sharedInstance().unregisterComponent(this);
			super.removeNotify();
		} 

		
		public String getToolTipText()
		{
			return new Date().toString();
		} 

		
		public Point getToolTipLocation(MouseEvent event)
		{
			return new Point(event.getX(),-20);
		} 

		
		public void actionPerformed(ActionEvent evt)
		{
			update();
		} 

		
		private Timer timer;

		
		private String getTime()
		{
			return DateFormat.getTimeInstance(
				DateFormat.SHORT).format(new Date());
		} 

		
		private void update()
		{
			setText(getTime());
		} 

		
	} 
}
