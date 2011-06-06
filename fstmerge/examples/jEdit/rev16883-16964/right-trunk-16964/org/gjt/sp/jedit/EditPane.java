

package org.gjt.sp.jedit;


import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import org.gjt.sp.jedit.EditBus.EBHandler;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.bufferset.BufferSet;
import org.gjt.sp.jedit.bufferset.BufferSetListener;
import org.gjt.sp.jedit.bufferset.BufferSetManager;
import org.gjt.sp.jedit.gui.BufferSwitcher;
import org.gjt.sp.jedit.gui.StatusBar;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.msg.BufferChanging;
import org.gjt.sp.jedit.msg.BufferUpdate;
import org.gjt.sp.jedit.msg.EditPaneUpdate;
import org.gjt.sp.jedit.msg.PropertiesChanged;
import org.gjt.sp.jedit.options.GutterOptionPane;
import org.gjt.sp.jedit.syntax.SyntaxStyle;
import org.gjt.sp.jedit.textarea.AntiAlias;
import org.gjt.sp.jedit.textarea.Gutter;
import org.gjt.sp.jedit.textarea.GutterPopupHandler;
import org.gjt.sp.jedit.textarea.JEditTextArea;
import org.gjt.sp.jedit.textarea.MouseHandler;
import org.gjt.sp.jedit.textarea.Selection;
import org.gjt.sp.jedit.textarea.StatusListener;
import org.gjt.sp.jedit.textarea.TextArea;
import org.gjt.sp.jedit.textarea.TextAreaExtension;
import org.gjt.sp.jedit.textarea.TextAreaPainter;
import org.gjt.sp.jedit.textarea.TextAreaTransferHandler;
import org.gjt.sp.util.SyntaxUtilities;




public class EditPane extends JPanel implements BufferSetListener
{
	
	
	public View getView()
	{
		return view;
	} 

	
	
	public static EditPane get(TextArea ta)
	{
		if (ta == null) return null;
		return (EditPane)SwingUtilities.getAncestorOfClass(EditPane.class, ta);
	} 

	
	
	public Buffer getBuffer()
	{
		return buffer;
	} 

	
	
	public void setBuffer(Buffer buffer)
	{
		setBuffer(buffer, true);
	}

	
	public void setBuffer(final Buffer buffer, boolean requestFocus)
	{

		if(buffer == null)
			throw new NullPointerException();

		if(this.buffer == buffer)
			return;

		if (bufferSet.indexOf(buffer) == -1)
		{
			jEdit.getBufferSetManager().addBuffer(this, buffer);
		}
		
		
		EditBus.send(new BufferChanging(this, buffer));
		if (bufferSet.indexOf(this.buffer) != -1)
		{
			
			
			
			recentBuffer = this.buffer;
		}
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
				bufferSwitcher.setToolTipText(buffer.getPath());
			}

			EditBus.send(new EditPaneUpdate(this,EditPaneUpdate
				.BUFFER_CHANGED));
		}

		if (requestFocus)
		{
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
		}

		
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
		Buffer buffer = bufferSet.getPreviousBuffer(bufferSet.indexOf(this.buffer));
		setBuffer(buffer);
	} 

	
	
	public void nextBuffer()
	{
		Buffer buffer = bufferSet.getNextBuffer(bufferSet.indexOf(this.buffer));
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

	
	
	public void focusBufferSwitcher()
	{
		if(bufferSwitcher == null)
			getToolkit().beep();
		else
		{
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					bufferSwitcher.requestFocus();
					bufferSwitcher.showPopup();
				}

			});
		}
	} 

	
	
	public void saveCaretInfo()
	{
		if(!buffer.isLoaded())
			return;

		buffer.setIntegerProperty(Buffer.CARET,
			textArea.getCaretPosition());

		CaretInfo caretInfo = caretsForPath.get(buffer.getPath());
		if (caretInfo == null)
		{
			caretInfo = new CaretInfo();
			caretsForPath.put(buffer.getPath(), caretInfo);
		}
		caretInfo.caret = textArea.getCaretPosition();


		Selection[] selection = textArea.getSelection();
		for(int i = 0; i < selection.length; i++)
			selection[i] = (Selection)selection[i].clone();
		buffer.setProperty(Buffer.SELECTION,selection);
		caretInfo.selection = selection;

		caretInfo.rectangularSelection = textArea.isRectangularSelectionEnabled();
		caretInfo.multipleSelection = textArea.isMultipleSelectionEnabled();

		buffer.setIntegerProperty(Buffer.SCROLL_VERT,
			textArea.getFirstPhysicalLine());
		caretInfo.scrollVert = textArea.getFirstPhysicalLine();
		buffer.setIntegerProperty(Buffer.SCROLL_HORIZ,
			textArea.getHorizontalOffset());
		caretInfo.scrollHoriz = textArea.getHorizontalOffset();
		if (!buffer.isUntitled())
		{
			BufferHistory.setEntry(buffer.getPath(), textArea.getCaretPosition(),
				(Selection[])buffer.getProperty(Buffer.SELECTION),
				buffer.getStringProperty(JEditBuffer.ENCODING),
				buffer.getMode().getName());
		}
	} 

	
	
	public void loadCaretInfo()
	{
		
		
		CaretInfo caretInfo = caretsForPath.get(buffer.getPath());
		if (caretInfo == null)
		{
			caretInfo = new CaretInfo();
		}

		
		
		
		
		
		int caret = caretInfo.caret;
		if (caret == -1 || buffer.getBooleanProperty(Buffer.CARET_POSITIONED))
		{
			Integer i = (Integer) buffer.getProperty(Buffer.CARET);
			caret = i == null ? -1 : i;
		}
		buffer.unsetProperty(Buffer.CARET_POSITIONED);


		if(caret != -1)
			textArea.setCaretPosition(Math.min(caret,
				buffer.getLength()));

		
		Selection[] selection = caretInfo.selection;
		if ( selection == null )
		{
			selection = (Selection[]) buffer.getProperty(Buffer.SELECTION);
		}
		if(selection != null)
		{
			for(int i = 0; i < selection.length; i++)
			{
				Selection s = selection[i];
				int max = buffer.getLength();
				if(s.getStart() > max || s.getEnd() > max)
					selection[i] = null;
			}
		}
		textArea.setSelection(selection);
		textArea.setRectangularSelectionEnabled(caretInfo.rectangularSelection);
		textArea.setMultipleSelectionEnabled(caretInfo.multipleSelection);
		
		int firstLine = caretInfo.scrollVert;
		if ( firstLine == -1 )
		{
			Integer i = (Integer) buffer.getProperty(Buffer.SCROLL_VERT);
			firstLine = i == null ? -1 : i;
		}

		if(firstLine != -1)
			textArea.setFirstPhysicalLine(firstLine);

		
		int horizontalOffset = caretInfo.scrollHoriz;
		if (horizontalOffset == -1)
		{
			Integer i = (Integer) buffer.getProperty(Buffer.SCROLL_HORIZ);
			horizontalOffset = i == null ? -1 : i;
		}

		if(horizontalOffset != -1)
			textArea.setHorizontalOffset(horizontalOffset);

		
		view.getStatus().setMessage(null);
	} 

	
	
	void bufferRenamed(String oldPath, String newPath)
	{
		CaretInfo caretInfo = caretsForPath.remove(oldPath);
		if (caretInfo != null)
			caretsForPath.put(newPath, caretInfo);

	} 

	
	
	private static class CaretInfo
	{
		public int caret = -1;
		public Selection[] selection;
		public int scrollVert = -1;
		public int scrollHoriz = -1;
		public boolean rectangularSelection;
		public boolean multipleSelection;
	} 

	
	
	public void goToNextMarker(boolean select)
	{
		java.util.List<Marker> markers = buffer.getMarkers();
		if(markers.isEmpty())
		{
			getToolkit().beep();
			return;
		}

		Marker marker = null;

		int caret = textArea.getCaretPosition();

		for(int i = 0; i < markers.size(); i++)
		{
			Marker _marker = markers.get(i);
			if(_marker.getPosition() > caret)
			{
				marker = _marker;
				break;
			}
		}
		
		if(marker == null)
			marker = markers.get(0);

		if(select)
			textArea.extendSelection(caret,marker.getPosition());
		else if(!textArea.isMultipleSelectionEnabled())
			textArea.selectNone();
		textArea.moveCaretPosition(marker.getPosition());
	} 

	
	
	public void goToPrevMarker(boolean select)
	{
		java.util.List<Marker> markers = buffer.getMarkers();
		if(markers.isEmpty())
		{
			getToolkit().beep();
			return;
		}

		int caret = textArea.getCaretPosition();

		Marker marker = null;
		for(int i = markers.size() - 1; i >= 0; i--)
		{
			Marker _marker = markers.get(i);
			if(_marker.getPosition() < caret)
			{
				marker = _marker;
				break;
			}
		}

		if(marker == null)
			marker = markers.get(markers.size() - 1);

		if(select)
			textArea.extendSelection(caret,marker.getPosition());
		else if(!textArea.isMultipleSelectionEnabled())
			textArea.selectNone();
		textArea.moveCaretPosition(marker.getPosition());
	} 

	
	
	public void goToMarker(char shortcut, boolean select)
	{
		Marker marker = buffer.getMarker(shortcut);
		if(marker == null)
		{
			getToolkit().beep();
			return;
		}

		int pos = marker.getPosition();

		if(select)
			textArea.extendSelection(textArea.getCaretPosition(),pos);
		else if(!textArea.isMultipleSelectionEnabled())
			textArea.selectNone();
		textArea.moveCaretPosition(pos);
	} 

	
	
	public void addMarker()
	{
		int caretLine = textArea.getCaretLine();

		
		Selection[] selection = textArea.getSelection();
		for(int i = 0; i < selection.length; i++)
		{
			Selection s = selection[i];
			int startLine = s.getStartLine();
			if(startLine != s.getEndLine() && startLine != caretLine)
			{
				buffer.addMarker('\0',s.getStart());
			}

			if(s.getEndLine() != caretLine)
				buffer.addMarker('\0',s.getEnd());
		}

		
		buffer.addOrRemoveMarker('\0',textArea.getCaretPosition());
	} 

	
	
	public void swapMarkerAndCaret(char shortcut)
	{
		Marker marker = buffer.getMarker(shortcut);
		if(marker == null)
		{
			getToolkit().beep();
			return;
		}

		int caret = textArea.getCaretPosition();

		textArea.setCaretPosition(marker.getPosition());
		buffer.addMarker(shortcut,caret);
	} 

	
	@EBHandler
	public void handlePropertiesChanged(PropertiesChanged msg)
	{
		propertiesChanged();
		loadBufferSwitcher();
	} 

	
	
	@Override
	public final Dimension getMinimumSize()
	{
		return new Dimension(0,0);
	} 

	
	
	public BufferSet getBufferSet()
	{
		return bufferSet;
	} 

	
	
	public BufferSet.Scope getBufferSetScope()
	{
		return bufferSetScope;
	} 

	
	
	public void setBufferSetScope(BufferSet.Scope scope)
	{
		if (this.bufferSetScope != scope)
		{
			BufferSet oldBufferSet = this.bufferSet;
			BufferSet newBufferSet;
			switch (scope)
			{
				case editpane:
					newBufferSet = new BufferSet();
					break;
				case view:
					newBufferSet = view.getLocalBufferSet();
					break;
				default:
					scope = BufferSet.Scope.global;
				case global:
					newBufferSet = jEdit.getGlobalBufferSet();
					break;
			}

			BufferSetManager bufferSetManager = jEdit.getBufferSetManager();

			if (jEdit.isStartupDone())	
			{
				String action = jEdit.getProperty("editpane.bufferset.new");
				BufferSetManager.NewBufferSetAction bufferSetAction = BufferSetManager.NewBufferSetAction.fromString(action);
				View activeView = jEdit.getActiveView();
				switch (bufferSetAction)
				{
					case copy:
						if (oldBufferSet == null)
						{
							EditPane editPane = view.getEditPane();
							if (editPane == null)
							{
								if (activeView != null)
									editPane = activeView.getEditPane();

							}
							if (editPane == null)
							{
								bufferSetManager.addAllBuffers(newBufferSet);
							}
							else
							{
								bufferSetManager.mergeBufferSet(newBufferSet, editPane.bufferSet);
							}
						}
						else
							bufferSetManager.mergeBufferSet(newBufferSet, oldBufferSet);
						break;
					case empty:
						break;
					case currentbuffer:
						if (activeView == null)
							break;
						EditPane editPane = activeView.getEditPane();
						Buffer buffer = editPane.getBuffer();
						bufferSetManager.addBuffer(newBufferSet,buffer);
						break;
				}
			}
			if (buffer != null)
				bufferSetManager.addBuffer(newBufferSet, buffer);


			this.bufferSet = newBufferSet;
			this.bufferSetScope = scope;
			if (newBufferSet.size() == 0)
			{
				jEdit.newFile(this);
			}

			
			
			
			if (oldBufferSet != null)
			{
				oldBufferSet.removeBufferSetListener(this);
			}

			newBufferSet.addBufferSetListener(this);
			if (bufferSwitcher != null)
			{
				bufferSwitcher.updateBufferList();
			}
			EditBus.send(new EditPaneUpdate(this, EditPaneUpdate.BUFFERSET_CHANGED));
			if (newBufferSet.indexOf(recentBuffer) == -1)
			{
				
				recentBuffer =  null;
			}
			if (newBufferSet.indexOf(buffer) == -1)
			{
				
				if (recentBuffer != null)
					setBuffer(recentBuffer);
				else
				{
					setBuffer(newBufferSet.getBuffer(0));
				}
			}
			if (jEdit.isStartupDone())	
				PerspectiveManager.setPerspectiveDirty(true);
		}
	} 

	
	
	public void bufferAdded(Buffer buffer, int index)
	{
		if (buffer == null)
			return;
		if (bufferSwitcher != null)
			bufferSwitcher.updateBufferList();
		if (bufferSet.indexOf(this.buffer) == -1)
		{
			
			
			setBuffer(buffer);
		}
	} 

	
	
	public void bufferRemoved(Buffer buffer, int index)
	{
		if (buffer.isUntitled())
		{
			
			caretsForPath.remove(buffer.getPath());
		}
		if (buffer == this.buffer)
		{
			
			Buffer newBuffer = recentBuffer != null ?
				recentBuffer : bufferSet.getPreviousBuffer(index);

			if(newBuffer != null && !newBuffer.isClosed())
			{
				setBuffer(newBuffer);
				if (bufferSet.size() > 1)
				{
					recentBuffer = bufferSet.getPreviousBuffer(index -1);
				}
			}
			else if(bufferSet.size() != 0)
			{
				setBuffer(bufferSet.getBuffer(0));
				recentBuffer = null;
			}
		}
		if(buffer == recentBuffer)
			recentBuffer = null;
		if (bufferSwitcher != null)
			bufferSwitcher.updateBufferList();
	} 

	
	
	public void bufferMoved(Buffer buffer, int oldIndex, int newIndex)
	{
		if (bufferSwitcher != null)
			bufferSwitcher.updateBufferList();
	} 

	
	
	public void bufferSetSorted()
	{
		if (bufferSwitcher != null)
			bufferSwitcher.updateBufferList();
	} 

	
	@Override
	public String toString()
	{
		return getClass().getName() + '['
			+ (view.getEditPane() == this
			? "active" : "inactive")
			+ ',' + bufferSetScope + ']';
	} 

	

	
	EditPane(View view, Buffer buffer, BufferSet.Scope scope)
	{
		super(new BorderLayout());

		init = true;

		this.view = view;


		textArea = new JEditTextArea(view);
		textArea.getPainter().setAntiAlias(new AntiAlias(jEdit.getProperty("view.antiAlias")));
		textArea.setMouseHandler(new MouseHandler(textArea));
		textArea.setTransferHandler(new TextAreaTransferHandler());
		markerHighlight = new MarkerHighlight();
		Gutter gutter = textArea.getGutter();
		gutter.setGutterEnabled(GutterOptionPane.isGutterEnabled());
		gutter.setMinLineNumberDigitCount(GutterOptionPane.getMinLineNumberDigits());
		gutter.setSelectionAreaEnabled(GutterOptionPane.isSelectionAreaEnabled());
		gutter.addExtension(markerHighlight);
		gutter.setSelectionPopupHandler(
			new GutterPopupHandler()
			{
				public void handlePopup(int x, int y, int line)
				{
					Buffer buffer = getBuffer();
					buffer.addOrRemoveMarker('\0',
						buffer.getLineStartOffset(line));
				}
			});

		textArea.addStatusListener(new StatusHandler());
		add(BorderLayout.CENTER,textArea);

		propertiesChanged();
		this.buffer = buffer;
		setBufferSetScope(scope);
		this.buffer = null;
		if(buffer == null)
		{
			setBuffer(jEdit.getFirstBuffer());
		}
		else
			setBuffer(buffer);

		loadBufferSwitcher();

		init = false;
		EditBus.addToBus(this);
	} 

	
	void close()
	{
		saveCaretInfo();
		EditBus.send(new EditPaneUpdate(this,EditPaneUpdate.DESTROYED));
		EditBus.removeFromBus(this);
		textArea.dispose();
	} 


	

	

	
	private boolean init;
	
	private final View view;

	private BufferSet bufferSet;
	private BufferSet.Scope bufferSetScope;

	
	private Buffer buffer;
	private Buffer recentBuffer;
	private BufferSwitcher bufferSwitcher;

	
	private final JEditTextArea textArea;
	private final MarkerHighlight markerHighlight;

	
	
	
	
	
	private final Map<String, CaretInfo> caretsForPath = new HashMap<String, CaretInfo>();

	

	
	private void propertiesChanged()
	{
		TextAreaPainter painter = textArea.getPainter();

		initPainter(painter);
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
		markerHighlight.setMarkerHighlightColor(
			jEdit.getColorProperty("view.gutter.markerColor"));
		markerHighlight.setMarkerHighlightEnabled(jEdit.getBooleanProperty(
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
		gutter.setGutterEnabled(GutterOptionPane.isGutterEnabled());
		gutter.setMinLineNumberDigitCount(
			GutterOptionPane.getMinLineNumberDigits());
		gutter.setSelectionAreaEnabled(
			GutterOptionPane.isSelectionAreaEnabled());
		gutter.setSelectionAreaBackground(
			GutterOptionPane.getSelectionAreaBackground());
		gutter.setSelectionAreaWidth(
				GutterOptionPane.getSelectionAreaWidth());

		int width = jEdit.getIntegerProperty(
			"view.gutter.borderWidth",3);
		gutter.setBorder(width,
			jEdit.getColorProperty("view.gutter.focusBorderColor"),
			jEdit.getColorProperty("view.gutter.noFocusBorderColor"),
			textArea.getPainter().getBackground());
		gutter.setFoldPainter(textArea.getFoldPainter());

		textArea.setCaretBlinkEnabled(jEdit.getBooleanProperty(
			"view.caretBlink"));

		textArea.setElectricScroll(jEdit.getIntegerProperty(
			"view.electricBorders",0));

		
		textArea.createPopupMenu(null);

		
		textArea.setQuickCopyEnabled(jEdit.getBooleanProperty(
			"view.middleMousePaste"));

		textArea.setDragEnabled(jEdit.getBooleanProperty(
			"view.dragAndDrop"));

		textArea.setJoinNonWordChars(jEdit.getBooleanProperty(
			"view.joinNonWordChars"));

		textArea.setCtrlForRectangularSelection(jEdit.getBooleanProperty(
			"view.ctrlForRectangularSelection"));

		textArea.propertiesChanged();

		if (bufferSwitcher != null)
		{
			bufferSwitcher.setMaximumRowCount(jEdit.getIntegerProperty(
				"bufferSwitcher.maxRowCount",10));
		}
	} 

	
	
	public static void initPainter(TextAreaPainter painter)
	{
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
		painter.setThickCaretEnabled(jEdit.getBooleanProperty(
			"view.thickCaret"));
		painter.setLineHighlightEnabled(jEdit.getBooleanProperty(
			"view.lineHighlight"));
		painter.setLineHighlightColor(
			jEdit.getColorProperty("view.lineHighlightColor"));
		painter.setAntiAlias(new AntiAlias(jEdit.getProperty("view.antiAlias")));
		painter.setFractionalFontMetricsEnabled(jEdit.getBooleanProperty(
			"view.fracFontMetrics"));

		painter.setSelectionFgColor(jEdit.getColorProperty(
			"view.selectionFgColor"));
		painter.setSelectionFgColorEnabled(jEdit.getBooleanProperty(
			"view.selectionFg"));

		String defaultFont = jEdit.getProperty("view.font");
		int defaultFontSize = jEdit.getIntegerProperty("view.fontsize",12);
		painter.setStyles(SyntaxUtilities.loadStyles(defaultFont,defaultFontSize));

		SyntaxStyle[] foldLineStyle = new SyntaxStyle[4];
		for(int i = 0; i <= 3; i++)
		{
			foldLineStyle[i] = GUIUtilities.parseStyle(
				jEdit.getProperty("view.style.foldLine." + i),
				defaultFont,defaultFontSize);
		}
		painter.setFoldLineStyle(foldLineStyle);
	} 

	
	void loadBufferSwitcher()
	{
		if(jEdit.getBooleanProperty("view.showBufferSwitcher"))
		{
			if(bufferSwitcher == null)
			{
				bufferSwitcher = new BufferSwitcher(this);
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

	
	@EBHandler
	public void handleBufferUpdate(BufferUpdate msg)
	{
		Buffer _buffer = msg.getBuffer();
		if(msg.getWhat() == BufferUpdate.CREATED)
		{
			if(bufferSwitcher != null)
				bufferSwitcher.updateBufferList();

			
			if(buffer.isClosed())
			{
				
				
				recentBuffer = null;
			}
		}
		else if(msg.getWhat() == BufferUpdate.CLOSED)
		{
			if(bufferSwitcher != null)
				bufferSwitcher.updateBufferList();

			if(_buffer == buffer)
			{
				
				Buffer newBuffer = recentBuffer != null ?
					recentBuffer : _buffer.getPrev();

				if(newBuffer != null && !newBuffer.isClosed())
				{
					setBuffer(newBuffer);
					recentBuffer = newBuffer.getPrev();
				}
			}
			else if(_buffer == recentBuffer)
				recentBuffer = null;

			Buffer closedBuffer = msg.getBuffer();
			if (closedBuffer.isUntitled())
			{
				
				caretsForPath.remove(closedBuffer.getPath());
			}
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
			if(_buffer == buffer && bufferSwitcher != null)
			{
				if(buffer.isDirty())
					bufferSwitcher.repaint();
				else
					bufferSwitcher.updateBufferList();
			}
		}
		else if(msg.getWhat() == BufferUpdate.MARKERS_CHANGED)
		{
			if(_buffer == buffer)
				textArea.getGutter().repaint();
		}
		else if(msg.getWhat() == BufferUpdate.PROPERTIES_CHANGED)
		{
			if(_buffer == buffer && buffer.isLoaded())
			{
				textArea.propertiesChanged();
				if(view.getEditPane() == this)
					view.getStatus().updateBufferStatus();
			}
		}
		else if(msg.getWhat() == BufferUpdate.SAVED && _buffer == buffer)
		{
			textArea.propertiesChanged();
		}
	} 

	

	
	class StatusHandler implements StatusListener
	{
		public void statusChanged(org.gjt.sp.jedit.textarea.TextArea textArea, int flag, boolean value)
		{
			StatusBar status = view.getStatus();
			if(status == null)
				return;

			switch(flag)
			{
			case OVERWRITE_CHANGED:
				status.setMessageAndClear(
					jEdit.getProperty("view.status.overwrite-changed",
					new Integer[] { value ? 1 : 0 }));
				break;
			case MULTI_SELECT_CHANGED:
				status.setMessageAndClear(
					jEdit.getProperty("view.status.multi-changed",
					new Integer[] { value ? 1 : 0 }));
				break;
			case RECT_SELECT_CHANGED:
				status.setMessageAndClear(
					jEdit.getProperty("view.status.rect-select-changed",
					new Integer[] { value ? 1 : 0 }));
				break;
			}

			status.updateMiscStatus();
		}

		public void bracketSelected(org.gjt.sp.jedit.textarea.TextArea textArea, int line, String text)
		{
			StatusBar status = view.getStatus();
			if(status == null)
				return;

			status.setMessageAndClear(jEdit.getProperty(
				"view.status.bracket",new Object[] {
				line, text }));
		}

		public void narrowActive(org.gjt.sp.jedit.textarea.TextArea textArea)
		{
			StatusBar status = view.getStatus();
			if(status == null)
				return;

			status.setMessageAndClear(
				jEdit.getProperty("view.status.narrow"));
		}
	} 

	
	class MarkerHighlight extends TextAreaExtension
	{
		private boolean markerHighlight;
		private Color markerHighlightColor;

		
		public Color getMarkerHighlightColor()
		{
			return markerHighlightColor;
		} 

		
		public void setMarkerHighlightColor(Color markerHighlightColor)
		{
			this.markerHighlightColor = markerHighlightColor;
		} 

		
		public boolean isMarkerHighlightEnabled()
		{
			return markerHighlight;
		} 

		
		public void setMarkerHighlightEnabled(boolean markerHighlight)
		{
			this.markerHighlight = markerHighlight;
		} 

		
		@Override
		public void paintValidLine(Graphics2D gfx, int screenLine,
			int physicalLine, int start, int end, int y)
		{
			if(isMarkerHighlightEnabled())
			{
				Buffer buffer = (Buffer)textArea.getBuffer();
				if(buffer.getMarkerInRange(start,end) != null)
				{
					gfx.setColor(getMarkerHighlightColor());
					FontMetrics fm = textArea.getPainter().getFontMetrics();
					gfx.fillRect(0,y,textArea.getGutter()
						.getWidth(),fm.getHeight());
				}
			}
		} 

		
		@Override
		public String getToolTipText(int x, int y)
		{
			if(isMarkerHighlightEnabled())
			{
				int lineHeight = textArea.getPainter().getFontMetrics().getHeight();
				if(lineHeight == 0)
					return null;

				int line = y / lineHeight;
				int start = textArea.getScreenLineStartOffset(line);
				int end = textArea.getScreenLineEndOffset(line);
				if(start == -1 || end == -1)
					return null;

				Buffer buffer = (Buffer)textArea.getBuffer();
				Marker marker = buffer.getMarkerInRange(start,end);
				if(marker != null)
				{
					char shortcut = marker.getShortcut();
					if(shortcut == '\0')
						return jEdit.getProperty("view.gutter.marker.no-name");
					else
					{
						String[] args = { String.valueOf(shortcut) };
						return jEdit.getProperty("view.gutter.marker",args);
					}
				}
			}

			return null;
		} 
	} 
}
