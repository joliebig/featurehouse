

package org.gjt.sp.jedit;


import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.lang.reflect.Method;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.options.GlobalOptions;
import org.gjt.sp.jedit.syntax.SyntaxStyle;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.util.Log;



public class EditPane extends JPanel implements EBComponent
{
	
	
	public View getView()
	{
		return view;
	} 

	
	
	public Buffer getBuffer()
	{
		return buffer;
	} 

	
	
	public void setBuffer(final Buffer buffer)
	{
		if(buffer == null)
			throw new NullPointerException();

		if(this.buffer == buffer)
			return;

		
		

		recentBuffer = this.buffer;
		if(recentBuffer != null)
			saveCaretInfo();
		this.buffer = buffer;

		textArea.setBuffer(buffer);

		if(!init)
		{
			view.updateTitle();

			if(bufferSwitcher != null)
			{
				if(bufferSwitcher.getSelectedItem() != buffer)
					bufferSwitcher.setSelectedItem(buffer);
			}

			EditBus.send(new EditPaneUpdate(this,EditPaneUpdate
				.BUFFER_CHANGED));
		}

		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				
				if(view.getEditPane() == EditPane.this
					&& (bufferSwitcher == null
					|| !bufferSwitcher.isPopupVisible()))
				{
					textArea.requestFocus();
				}
			}
		});

		
		Runnable runnable = new Runnable()
		{
			public void run()
			{
				
				
				if(buffer == getBuffer())
					loadCaretInfo();
			}
		};

		if(buffer.isPerformingIO())
			VFSManager.runInAWTThread(runnable);
		else
			runnable.run();
	} 

	
	
	public void prevBuffer()
	{
		Buffer buffer = this.buffer.getPrev();
		if(buffer == null)
			setBuffer(jEdit.getLastBuffer());
		else
			setBuffer(buffer);
	} 

	
	
	public void nextBuffer()
	{
		Buffer buffer = this.buffer.getNext();
		if(buffer == null)
			setBuffer(jEdit.getFirstBuffer());
		else
			setBuffer(buffer);
	} 

	
	
	public void recentBuffer()
	{
		if(recentBuffer != null)
			setBuffer(recentBuffer);
		else
			getToolkit().beep();
	} 

	
	
	public void focusOnTextArea()
	{
		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				textArea.requestFocus();
			}
		});
	} 

	
	
	public JEditTextArea getTextArea()
	{
		return textArea;
	} 

	
	
	public BufferSwitcher getBufferSwitcher()
	{
		return bufferSwitcher;
	} 

	
	
	public void showBufferSwitcher()
	{
		if(bufferSwitcher == null)
			getToolkit().beep();
		else
		{
			bufferSwitcher.requestFocus();
			bufferSwitcher.showPopup();
		}
	} 

	
	
	public void saveCaretInfo()
	{
		buffer.setIntegerProperty(Buffer.CARET,
			textArea.getCaretPosition());

		

		buffer.setIntegerProperty(Buffer.SCROLL_VERT,
			textArea.getFirstPhysicalLine());
		buffer.setIntegerProperty(Buffer.SCROLL_HORIZ,
			textArea.getHorizontalOffset());
	} 

	
	
	public void loadCaretInfo()
	{
		Integer caret = (Integer)buffer.getProperty(Buffer.CARET);
		

		Integer firstLine = (Integer)buffer.getProperty(Buffer.SCROLL_VERT);
		Integer horizontalOffset = (Integer)buffer.getProperty(Buffer.SCROLL_HORIZ);

		if(caret != null)
		{
			textArea.setCaretPosition(Math.min(caret.intValue(),
				buffer.getLength()));
		}

		

		if(firstLine != null)
			textArea.setFirstPhysicalLine(firstLine.intValue());

		if(horizontalOffset != null)
			textArea.setHorizontalOffset(horizontalOffset.intValue());

		
		view.getStatus().setMessage(null);
	} 

	
	public void handleMessage(EBMessage msg)
	{
		if(msg instanceof PropertiesChanged)
		{
			propertiesChanged();
			loadBufferSwitcher();
		}
		else if(msg instanceof BufferUpdate)
			handleBufferUpdate((BufferUpdate)msg);
	} 

	
	
	public final Dimension getMinimumSize()
	{
		return new Dimension(0,0);
	} 

	
	public String toString()
	{
		return getClass().getName() + "["
			+ (view.getEditPane() == this
			? "active" : "inactive")
			+ "]";
	} 

	

	
	EditPane(View view, Buffer buffer)
	{
		super(new BorderLayout());

		init = true;

		this.view = view;

		EditBus.addToBus(this);

		textArea = new JEditTextArea(view);

		add(BorderLayout.CENTER,textArea);

		propertiesChanged();

		if(buffer == null)
			setBuffer(jEdit.getFirstBuffer());
		else
			setBuffer(buffer);

		loadBufferSwitcher();

		init = false;
	} 

	
	void close()
	{
		saveCaretInfo();
		EditBus.send(new EditPaneUpdate(this,EditPaneUpdate.DESTROYED));
		EditBus.removeFromBus(this);
		textArea.dispose();
	} 

	

	

	private static Method initBufferSwitcher;

	static
	{
		if(OperatingSystem.hasJava14())
		{
			try
			{
				initBufferSwitcher = Java14.class
					.getMethod("initBufferSwitcher",
					new Class[] { EditPane.class,
					BufferSwitcher.class });
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,EditPane.class,e);
			}
		}
	}

	
	private boolean init;
	private View view;
	private Buffer buffer;
	private Buffer recentBuffer;
	private BufferSwitcher bufferSwitcher;
	private JEditTextArea textArea;
	

	
	private void propertiesChanged()
	{
		TextAreaPainter painter = textArea.getPainter();

		painter.setFont(jEdit.getFontProperty("view.font"));
		painter.setStructureHighlightEnabled(jEdit.getBooleanProperty(
			"view.structureHighlight"));
		painter.setStructureHighlightColor(
			jEdit.getColorProperty("view.structureHighlightColor"));
		painter.setEOLMarkersPainted(jEdit.getBooleanProperty(
			"view.eolMarkers"));
		painter.setEOLMarkerColor(
			jEdit.getColorProperty("view.eolMarkerColor"));
		painter.setWrapGuidePainted(jEdit.getBooleanProperty(
			"view.wrapGuide"));
		painter.setWrapGuideColor(
			jEdit.getColorProperty("view.wrapGuideColor"));
		painter.setCaretColor(
			jEdit.getColorProperty("view.caretColor"));
		painter.setSelectionColor(
			jEdit.getColorProperty("view.selectionColor"));
		painter.setMultipleSelectionColor(
			jEdit.getColorProperty("view.multipleSelectionColor"));
		painter.setBackground(
			jEdit.getColorProperty("view.bgColor"));
		painter.setForeground(
			jEdit.getColorProperty("view.fgColor"));
		painter.setBlockCaretEnabled(jEdit.getBooleanProperty(
			"view.blockCaret"));
		painter.setLineHighlightEnabled(jEdit.getBooleanProperty(
			"view.lineHighlight"));
		painter.setLineHighlightColor(
			jEdit.getColorProperty("view.lineHighlightColor"));
		painter.setAntiAliasEnabled(jEdit.getBooleanProperty(
			"view.antiAlias"));
		painter.setFractionalFontMetricsEnabled(jEdit.getBooleanProperty(
			"view.fracFontMetrics"));

		String defaultFont = jEdit.getProperty("view.font");
		int defaultFontSize = jEdit.getIntegerProperty("view.fontsize",12);
		painter.setStyles(GUIUtilities.loadStyles(defaultFont,defaultFontSize));

		SyntaxStyle[] foldLineStyle = new SyntaxStyle[4];
		for(int i = 0; i <= 3; i++)
		{
			foldLineStyle[i] = GUIUtilities.parseStyle(
				jEdit.getProperty("view.style.foldLine." + i),
				defaultFont,defaultFontSize);
		}
		painter.setFoldLineStyle(foldLineStyle);
		Gutter gutter = textArea.getGutter();
		gutter.setExpanded(jEdit.getBooleanProperty(
			"view.gutter.lineNumbers"));
		int interval = jEdit.getIntegerProperty(
			"view.gutter.highlightInterval",5);
		gutter.setHighlightInterval(interval);
		gutter.setCurrentLineHighlightEnabled(jEdit.getBooleanProperty(
			"view.gutter.highlightCurrentLine"));
		gutter.setStructureHighlightEnabled(jEdit.getBooleanProperty(
			"view.gutter.structureHighlight"));
		gutter.setStructureHighlightColor(
			jEdit.getColorProperty("view.gutter.structureHighlightColor"));
		gutter.setBackground(
			jEdit.getColorProperty("view.gutter.bgColor"));
		gutter.setForeground(
			jEdit.getColorProperty("view.gutter.fgColor"));
		gutter.setHighlightedForeground(
			jEdit.getColorProperty("view.gutter.highlightColor"));
		gutter.setFoldColor(
			jEdit.getColorProperty("view.gutter.foldColor"));
		gutter.setMarkerHighlightColor(
			jEdit.getColorProperty("view.gutter.markerColor"));
		gutter.setMarkerHighlightEnabled(jEdit.getBooleanProperty(
			"view.gutter.markerHighlight"));
		gutter.setCurrentLineForeground(
			jEdit.getColorProperty("view.gutter.currentLineColor"));
		String alignment = jEdit.getProperty(
			"view.gutter.numberAlignment");
		if ("right".equals(alignment))
		{
			gutter.setLineNumberAlignment(Gutter.RIGHT);
		}
		else if ("center".equals(alignment))
		{
			gutter.setLineNumberAlignment(Gutter.CENTER);
		}
		else 
		{
			gutter.setLineNumberAlignment(Gutter.LEFT);
		}

		gutter.setFont(jEdit.getFontProperty("view.gutter.font"));

		int width = jEdit.getIntegerProperty(
			"view.gutter.borderWidth",3);
		gutter.setBorder(width,
			jEdit.getColorProperty("view.gutter.focusBorderColor"),
			jEdit.getColorProperty("view.gutter.noFocusBorderColor"),
			textArea.getPainter().getBackground());

		textArea.setCaretBlinkEnabled(jEdit.getBooleanProperty(
			"view.caretBlink"));

		textArea.setElectricScroll(jEdit.getIntegerProperty(
			"view.electricBorders",0));

		
		JPopupMenu popup = GUIUtilities.loadPopupMenu("view.context");
		JMenuItem customize = new JMenuItem(jEdit.getProperty(
			"view.context.customize"));
		customize.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				new GlobalOptions(view,"context");
			}
		});
		popup.addSeparator();
		popup.add(customize);
		textArea.setRightClickPopup(popup);

		
		textArea.setQuickCopyEnabled(jEdit.getBooleanProperty(
			"view.middleMousePaste"));

		textArea.setDragEnabled(jEdit.getBooleanProperty(
			"view.dragAndDrop"));

		textArea.propertiesChanged();
	} 

	
	private void loadBufferSwitcher()
	{
		if(jEdit.getBooleanProperty("view.showBufferSwitcher"))
		{
			if(bufferSwitcher == null)
			{
				bufferSwitcher = new BufferSwitcher(this);
				if(initBufferSwitcher != null)
				{
					try
					{
						initBufferSwitcher.invoke(
							null,new Object[] {
								EditPane.this,
								bufferSwitcher
							});
					}
					catch(Exception e)
					{
						Log.log(Log.ERROR,this,e);
					}
				}
				add(BorderLayout.NORTH,bufferSwitcher);
				bufferSwitcher.updateBufferList();
				revalidate();
			}
		}
		else if(bufferSwitcher != null)
		{
			remove(bufferSwitcher);
			revalidate();
			bufferSwitcher = null;
		}
	} 

	
	private void handleBufferUpdate(BufferUpdate msg)
	{
		Buffer _buffer = msg.getBuffer();
		if(msg.getWhat() == BufferUpdate.CREATED)
		{
			if(bufferSwitcher != null)
				bufferSwitcher.updateBufferList();

			
			if(buffer.isClosed())
			{
				setBuffer(jEdit.getFirstBuffer());
				
				
				recentBuffer = null;
			}
		}
		else if(msg.getWhat() == BufferUpdate.CLOSED)
		{
			if(bufferSwitcher != null)
				bufferSwitcher.updateBufferList();

			if(_buffer == buffer)
			{
				Buffer newBuffer = (recentBuffer != null ?
					recentBuffer : _buffer.getPrev());
				if(newBuffer != null && !newBuffer.isClosed())
					setBuffer(newBuffer);
				else if(jEdit.getBufferCount() != 0)
					setBuffer(jEdit.getFirstBuffer());

				recentBuffer = null;
			}
			else if(_buffer == recentBuffer)
				recentBuffer = null;
		}
		else if(msg.getWhat() == BufferUpdate.LOAD_STARTED)
		{
			if(_buffer == buffer)
			{
				textArea.setCaretPosition(0);
				textArea.getPainter().repaint();
			}
		}
		else if(msg.getWhat() == BufferUpdate.LOADED)
		{
			if(_buffer == buffer)
			{
				textArea.repaint();
				if(bufferSwitcher != null)
					bufferSwitcher.updateBufferList();

				if(view.getEditPane() == this)
				{
					StatusBar status = view.getStatus();
					status.updateCaretStatus();
					status.updateBufferStatus();
					status.updateMiscStatus();
				}

				loadCaretInfo();
			}

		}
		else if(msg.getWhat() == BufferUpdate.DIRTY_CHANGED)
		{
			if(_buffer == buffer)
			{
				if(bufferSwitcher != null)
				{
					if(buffer.isDirty())
						bufferSwitcher.repaint();
					else
						bufferSwitcher.updateBufferList();
				}
			}
		}
		else if(msg.getWhat() == BufferUpdate.MARKERS_CHANGED)
		{
			if(_buffer == buffer)
				textArea.getGutter().repaint();
		}
		else if(msg.getWhat() == BufferUpdate.PROPERTIES_CHANGED)
		{
			if(_buffer == buffer)
			{
				textArea.propertiesChanged();
				if(view.getEditPane() == this)
					view.getStatus().updateBufferStatus();
			}
		}
		else if(msg.getWhat() == BufferUpdate.SAVED)
		{
			if(_buffer == buffer)
				textArea.propertiesChanged();
		}
	} 

	
}
