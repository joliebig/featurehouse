

package org.gjt.sp.jedit.gui;


import javax.swing.border.*;
import javax.swing.text.Segment;
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.util.StringTokenizer;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.gui.statusbar.StatusWidgetFactory;
import org.gjt.sp.jedit.gui.statusbar.Widget;
import org.gjt.sp.jedit.gui.statusbar.ToolTipLabel;
import org.gjt.sp.util.*;



public class StatusBar extends JPanel implements WorkThreadProgressListener
{
	
	public StatusBar(View view)
	{
		super(new BorderLayout());
		setName("StatusBar");
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
		caretStatus.setName("caretStatus");
		caretStatus.setToolTipText(jEdit.getProperty("view.status.caret-tooltip"));
		caretStatus.addMouseListener(mouseHandler);

		message = new JLabel(" ");
		setMessageComponent(message);

		modeWidget = _getWidget("mode");
		foldWidget = _getWidget("fold");
		encodingWidget = _getWidget("encoding");
		wrapWidget = _getWidget("wrap");
		multiSelectWidget = _getWidget("multiSelect");
		rectSelectWidget = _getWidget("rectSelect");
		overwriteWidget = _getWidget("overwrite");
		lineSepWidget = _getWidget("lineSep");
	} 

	
	public void propertiesChanged()
	{
		Color fg = jEdit.getColorProperty("view.status.foreground");
		Color bg = jEdit.getColorProperty("view.status.background");

		showCaretStatus = jEdit.getBooleanProperty("view.status.show-caret-status");

		panel.setBackground(bg);
		panel.setForeground(fg);
		caretStatus.setBackground(bg);
		caretStatus.setForeground(fg);
		message.setBackground(bg);
		message.setForeground(fg);

		
		Font font = new JLabel().getFont();
		
		FontMetrics fm = getFontMetrics(font);

		if (showCaretStatus)
		{
			panel.add(BorderLayout.WEST,caretStatus);

			caretStatus.setFont(font);

			Dimension dim = new Dimension(fm.stringWidth(caretTestStr),
					fm.getHeight());
			caretStatus.setPreferredSize(dim);
			updateCaretStatus();
		}
		else
			panel.remove(caretStatus);

		String statusBar = jEdit.getProperty("view.status");
		if (!StandardUtilities.objectsEqual(currentBar, statusBar))
		{
			box.removeAll();
			StringTokenizer tokenizer = new StringTokenizer(statusBar);
			while (tokenizer.hasMoreTokens())
			{
				String token = tokenizer.nextToken();
				if (Character.isLetter(token.charAt(0)))
				{
					Widget widget = getWidget(token);
					if (widget == null)
					{
						JLabel label = new JLabel(token);
						label.setBackground(bg);
						label.setForeground(fg);
						box.add(label);
						continue;
					}
					Component c = widget.getComponent();
					c.setBackground(bg);
					c.setForeground(fg);
					box.add(c);
					widget.update();
					widget.propertiesChanged();
				}
				else
				{
					JLabel label = new JLabel(token);
					label.setBackground(bg);
					label.setForeground(fg);
					box.add(label);
				}
			}
			currentBar = statusBar;
		}
		updateBufferStatus();
		updateMiscStatus();
	} 

	
	@Override
	public void addNotify()
	{
		super.addNotify();
		VFSManager.getIOThreadPool().addProgressListener(this);
	} 

	
	@Override
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
					Object[] args = {requestCount};
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

			int caretPosition = textArea.getCaretPosition();
			int currLine = textArea.getCaretLine();

			
			
			
			
			
			if(currLine >= buffer.getLineCount())
				return; 

			int start = textArea.getLineStartOffset(currLine);
			int dot = caretPosition - start;

			if(dot < 0)
				return;

			int bufferLength = buffer.getLength();

			buffer.getText(start,dot,seg);
			int virtualPosition = StandardUtilities.getVirtualWidth(seg,
				buffer.getTabSize());
			
			seg.array = null;
			seg.count = 0;

			if (jEdit.getBooleanProperty("view.status.show-caret-linenumber", true))
			{
				buf.append(currLine + 1);
				buf.append(',');
			}
			if (jEdit.getBooleanProperty("view.status.show-caret-dot", true))
			{
				buf.append(dot + 1);
			}
			if (jEdit.getBooleanProperty("view.status.show-caret-virtual", true) &&
				virtualPosition != dot)
			{
				buf.append('-');
				buf.append(virtualPosition + 1);
			}
			if (buf.length() > 0)
			{
				buf.append(' ');
			}
			if (jEdit.getBooleanProperty("view.status.show-caret-offset", true) &&
				jEdit.getBooleanProperty("view.status.show-caret-bufferlength", true))
			{
				buf.append('(');
				buf.append(caretPosition);
				buf.append('/');
				buf.append(bufferLength);
				buf.append(')');
			}
			else if (jEdit.getBooleanProperty("view.status.show-caret-offset", true))
			{
				buf.append('(');
				buf.append(caretPosition);
				buf.append(')');
			}
			else if (jEdit.getBooleanProperty("view.status.show-caret-bufferlength", true))
			{
				buf.append('(');
				buf.append(bufferLength);
				buf.append(')');
			}

			caretStatus.setText(buf.toString());
			buf.setLength(0);
		}
	} 

	
	public void updateBufferStatus()
	{
		wrapWidget.update();
		lineSepWidget.update();
		modeWidget.update();
		foldWidget.update();
		encodingWidget.update();
	} 

	
	public void updateMiscStatus()
	{
		multiSelectWidget.update();
		rectSelectWidget.update();
		overwriteWidget.update();
	} 

	
	private String currentBar;
	private final View view;
	private final JPanel panel;
	private final Box box;
	private final ToolTipLabel caretStatus;
	private Component messageComp;
	private final JLabel message;
	private final Widget modeWidget;
	private final Widget foldWidget;
	private final Widget encodingWidget;
	private final Widget wrapWidget;
	private final Widget multiSelectWidget;
	private final Widget rectSelectWidget;
	private final Widget overwriteWidget;
	private final Widget lineSepWidget;
	 StringBuilder buf = new StringBuilder();
	private Timer tempTimer;
	private boolean currentMessageIsIO;

	private final Segment seg = new Segment();

	private boolean showCaretStatus;
	

	static final String caretTestStr = "9999,999-999 (99999999/99999999)";

	
	private Widget getWidget(String name)
	{
		if ("mode".equals(name))
			return modeWidget;
		if ("fold".equals(name))
			return foldWidget;
		if ("encoding".equals(name))
			return encodingWidget;
		if ("wrap".equals(name))
			return wrapWidget;
		if ("multiSelect".equals(name))
			return multiSelectWidget;
		if ("rectSelect".equals(name))
			return rectSelectWidget;
		if ("overwrite".equals(name))
			return overwriteWidget;
		if ("lineSep".equals(name))
			return lineSepWidget;

		return _getWidget(name);
	} 

	
	private Widget _getWidget(String name)
	{
		StatusWidgetFactory widgetFactory =
		(StatusWidgetFactory) ServiceManager.getService("org.gjt.sp.jedit.gui.statusbar.StatusWidget", name);
		if (widgetFactory == null)
		{
			return null;
		}
		return widgetFactory.getWidget(view);
	} 

	
	private class MouseHandler extends MouseAdapter
	{
		@Override
		public void mouseClicked(MouseEvent evt)
		{
			Object source = evt.getSource();
			if(source == caretStatus && evt.getClickCount() == 2)
			{
				view.getTextArea().showGoToLineDialog();
			}
		}
	} 
}
