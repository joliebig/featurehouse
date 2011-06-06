
package org.gjt.sp.jedit.textarea;


import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Properties;

import javax.swing.AbstractAction;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.WindowConstants;

import org.gjt.sp.jedit.IPropertyManager;
import org.gjt.sp.jedit.JEditBeanShellAction;
import org.gjt.sp.jedit.Mode;
import org.gjt.sp.jedit.JEditActionSet;
import org.gjt.sp.jedit.input.AbstractInputHandler;
import org.gjt.sp.jedit.buffer.DefaultFoldHandlerProvider;
import org.gjt.sp.jedit.buffer.DummyFoldHandler;
import org.gjt.sp.jedit.buffer.ExplicitFoldHandler;
import org.gjt.sp.jedit.buffer.FoldHandler;
import org.gjt.sp.jedit.buffer.IndentFoldHandler;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.buffer.KillRing;
import org.gjt.sp.jedit.syntax.ModeProvider;
import org.gjt.sp.jedit.syntax.ParserRuleSet;
import org.gjt.sp.jedit.syntax.SyntaxStyle;
import org.gjt.sp.jedit.syntax.TokenMarker;
import org.gjt.sp.util.IOUtilities;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.SyntaxUtilities;




public class StandaloneTextArea extends TextArea
{

	
	private final IPropertyManager propertyManager;
	

	
	
	public StandaloneTextArea(IPropertyManager propertyManager)
	{
		super(propertyManager, null);
		this.propertyManager = propertyManager;

		initInputHandler();

		setMouseHandler(new TextAreaMouseHandler(this));
		


		JEditActionSet<JEditBeanShellAction> actionSet = new StandaloneActionSet(propertyManager, this, TextArea.class.getResource("textarea.actions.xml"));

		addActionSet(actionSet);
		actionSet.load();
		actionSet.initKeyBindings();

		
		if (SyntaxUtilities.propertyManager == null)
		{
			SyntaxUtilities.propertyManager = propertyManager;
		}
		

		initTextArea();

		DefaultFoldHandlerProvider foldHandlerProvider = new DefaultFoldHandlerProvider();

		FoldHandler.foldHandlerProvider = foldHandlerProvider;
		foldHandlerProvider.addFoldHandler(new ExplicitFoldHandler());
		foldHandlerProvider.addFoldHandler(new IndentFoldHandler());
		foldHandlerProvider.addFoldHandler(new DummyFoldHandler());
		JEditBuffer buffer = new JEditBuffer();
		TokenMarker tokenMarker = new TokenMarker();
		tokenMarker.addRuleSet(new ParserRuleSet("text","MAIN"));
		buffer.setTokenMarker(tokenMarker);
		setBuffer(buffer);
		String property = propertyManager.getProperty("buffer.undoCount");
		int undoCount = 100;
		if (property != null)
			try
			{
				undoCount = Integer.parseInt(property);
			}
			catch (NumberFormatException e)
			{
			}
		this.buffer.setUndoLimit(undoCount);
		Mode mode = new Mode("text");
		mode.setTokenMarker(tokenMarker);
		ModeProvider.instance.addMode(mode);
		KillRing.setInstance(new KillRing());
		KillRing.getInstance().propertiesChanged(100);

	} 

	
	
	private void initTextArea()
	{
		initPainter();
		initGutter();

		setCaretBlinkEnabled(getBooleanProperty(
			"view.caretBlink"));

		setElectricScroll(getIntegerProperty(
			"view.electricBorders",0));

		if (buffer == null)
			return ;

		String property = propertyManager.getProperty("buffer.undoCount");
		int undoCount = 100;
		if (property != null)
		{
			try
			{
				undoCount = Integer.parseInt(property);
			}
			catch (NumberFormatException e)
			{
			}
		}
		buffer.setUndoLimit(undoCount);
	} 

	
	private void initGutter()
	{
		Gutter gutter = getGutter();
		gutter.setExpanded(getBooleanProperty(
			"view.gutter.lineNumbers"));
		int interval = getIntegerProperty(
			"view.gutter.highlightInterval",5);
		gutter.setHighlightInterval(interval);
		gutter.setCurrentLineHighlightEnabled(getBooleanProperty(
			"view.gutter.highlightCurrentLine"));
		gutter.setStructureHighlightEnabled(getBooleanProperty(
			"view.gutter.structureHighlight"));
		gutter.setStructureHighlightColor(
			getColorProperty("view.gutter.structureHighlightColor"));
		gutter.setBackground(
			getColorProperty("view.gutter.bgColor"));
		gutter.setForeground(
			getColorProperty("view.gutter.fgColor"));
		gutter.setHighlightedForeground(
			getColorProperty("view.gutter.highlightColor"));
		gutter.setFoldColor(
			getColorProperty("view.gutter.foldColor"));
		gutter.setCurrentLineForeground(
			getColorProperty("view.gutter.currentLineColor"));
		String alignment = getProperty(
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

		gutter.setFont(getFontProperty("view.gutter.font"));

		int width = getIntegerProperty(
			"view.gutter.borderWidth",3);
		gutter.setBorder(width,
			getColorProperty("view.gutter.focusBorderColor"),
			getColorProperty("view.gutter.noFocusBorderColor"),
			painter.getBackground());
	} 

	
	
	private void initPainter()
	{
		TextAreaPainter painter = getPainter();
		painter.setBlockCaretEnabled(false);

		painter.setFont(getFontProperty("view.font"));
		painter.setStructureHighlightEnabled(getBooleanProperty(
			"view.structureHighlight"));
		painter.setStructureHighlightColor(
			getColorProperty("view.structureHighlightColor"));
		painter.setEOLMarkersPainted(getBooleanProperty(
			"view.eolMarkers"));
		painter.setEOLMarkerColor(
			getColorProperty("view.eolMarkerColor"));
		painter.setWrapGuidePainted(getBooleanProperty(
			"view.wrapGuide"));
		painter.setWrapGuideColor(
			getColorProperty("view.wrapGuideColor"));
		painter.setCaretColor(
			getColorProperty("view.caretColor"));
		painter.setSelectionColor(
			getColorProperty("view.selectionColor"));
		painter.setMultipleSelectionColor(
			getColorProperty("view.multipleSelectionColor"));
		painter.setBackground(
			getColorProperty("view.bgColor"));
		painter.setForeground(
			getColorProperty("view.fgColor"));
		painter.setBlockCaretEnabled(getBooleanProperty(
			"view.blockCaret"));
		painter.setThickCaretEnabled(getBooleanProperty(
			"view.thickCaret"));
		painter.setLineHighlightEnabled(getBooleanProperty(
			"view.lineHighlight"));
		painter.setLineHighlightColor(
			getColorProperty("view.lineHighlightColor"));
		painter.setAntiAlias(new AntiAlias(getProperty("view.antiAlias")));
		painter.setFractionalFontMetricsEnabled(getBooleanProperty(
			"view.fracFontMetrics"));
		painter.setSelectionFgColor(getColorProperty(
			"view.selectionFgColor"));
		painter.setSelectionFgColorEnabled(getBooleanProperty(
			"view.selectionFg"));
		String defaultFont = getProperty("view.font");
		int defaultFontSize = getIntegerProperty("view.fontsize",12);
		painter.setStyles(SyntaxUtilities.loadStyles(defaultFont,defaultFontSize));

		SyntaxStyle[] foldLineStyle = new SyntaxStyle[4];
		for(int i = 0; i <= 3; i++)
		{
			foldLineStyle[i] = SyntaxUtilities.parseStyle(
				getProperty("view.style.foldLine." + i),
				defaultFont,defaultFontSize,true);
		}
		painter.setFoldLineStyle(foldLineStyle);
	} 

	
	
	
	

	
	public String getProperty(String name)
	{
		return propertyManager.getProperty(name);
	} 

	
	
	private boolean getBooleanProperty(String name)
	{
		return getBooleanProperty(name,false);
	} 

	
	
	private boolean getBooleanProperty(String name, boolean def)
	{
		String value = getProperty(name);
		if(value == null)
			return def;
		else if(value.equals("true") || value.equals("yes")
			|| value.equals("on"))
			return true;
		else if(value.equals("false") || value.equals("no")
			|| value.equals("off"))
			return false;
		else
			return def;
	} 

	
	
	private int getIntegerProperty(String name, int def)
	{
		String value = getProperty(name);
		if(value == null)
			return def;
		else
		{
			try
			{
				return Integer.parseInt(value.trim());
			}
			catch(NumberFormatException nf)
			{
				return def;
			}
		}
	} 


	
	
	private Font getFontProperty(String name)
	{
		return getFontProperty(name,null);
	}

	
	private Font getFontProperty(String name, Font def)
	{
		String family = getProperty(name);
		String sizeString = getProperty(name + "size");
		String styleString = getProperty(name + "style");

		if(family == null || sizeString == null || styleString == null)
			return def;
		else
		{
			int size;

			try
			{
				size = Integer.parseInt(sizeString);
			}
			catch(NumberFormatException nf)
			{
				return def;
			}

			int style;
			try
			{
				style = Integer.parseInt(styleString);
			}
			catch(NumberFormatException nf)
			{
				return def;
			}

			return new Font(family,style,size);
		}
	} 

	
	
	private Color getColorProperty(String name)
	{
		return getColorProperty(name,Color.black);
	}

	
	private Color getColorProperty(String name, Color def)
	{
		String value = getProperty(name);
		if(value == null)
			return def;
		else
			return SyntaxUtilities.parseColor(value, def);
	} 


	
	
	@Override
	public void propertiesChanged()
	{
		getInputHandler().removeAllKeyBindings();
		JEditActionSet<JEditBeanShellAction>[] actionSets = getActionContext().getActionSets();
		for (JEditActionSet<JEditBeanShellAction> actionSet : actionSets)
		{
			actionSet.initKeyBindings();
		}
		initBuffer();
		initTextArea();
		super.propertiesChanged();
	} 

	
	
	private void initBuffer()
	{
		String[] bufferProperties = {
			"lineSeparator",
			"encodingAutodetect",
			"tabSize",
			"indentSize",
			"noTabs",
			"defaultMode",
			"undoCount",
			"wrap",
			"maxLineLen",
			"wordBreakChars",
			"noWordSep",
			"camelCasedWords",
			"folding",
			"collapseFolds"
		};
		for (int i = 0; i < bufferProperties.length; i++)
		{
			String value = getProperty("buffer." + bufferProperties[i]);
			if (value == null)
				buffer.unsetProperty(bufferProperties[i]);
			else
				buffer.setProperty(bufferProperties[i], value);
		}
		buffer.propertiesChanged();
	} 

	
	
	@Override
	public void createPopupMenu(MouseEvent evt)
	{
		popup = new JPopupMenu();
		addMenuItem("undo", "Undo");
		addMenuItem("redo", "Redo");
		popup.addSeparator();
		addMenuItem("cut", "Cut");
		addMenuItem("copy", "Copy");
		addMenuItem("paste", "Paste");
	} 

	
	
	public JMenuItem addMenuItem(String action, String label)
	{
		final JEditBeanShellAction shellAction = getActionContext().getAction(action);
		if (shellAction == null)
			return null ;
		JMenuItem item = new JMenuItem();
		item.setAction(new AbstractAction(label)
		{
			public void actionPerformed(ActionEvent e)
			{
				shellAction.invoke(StandaloneTextArea.this);
			}
		});
		popup.add(item);
		return item;
	} 

	
	
	public static StandaloneTextArea createTextArea()
	{
		final Properties props = new Properties();
		props.putAll(loadProperties("/org/gjt/sp/jedit/jedit_keys.props"));
		props.putAll(loadProperties("/org/gjt/sp/jedit/jedit.props"));
		StandaloneTextArea textArea = new StandaloneTextArea(new IPropertyManager()
		{
			public String getProperty(String name)
			{
				return props.getProperty(name);
			}
		});
		textArea.getBuffer().setProperty("folding", "explicit");
		return textArea;
	} 

	
	private static Properties loadProperties(String fileName)
	{
		Properties props = new Properties();
		InputStream in = TextArea.class.getResourceAsStream(fileName);
		try
		{
			props.load(in);
		}
		catch (IOException e)
		{
			Log.log(Log.ERROR, TextArea.class, e);
		}
		finally
		{
			IOUtilities.closeQuietly(in);
		}
		return props;
	} 

	
	
	protected static class StandaloneActionSet extends JEditActionSet<JEditBeanShellAction>
	{
		private final IPropertyManager iPropertyManager;
		private final TextArea textArea;

		public StandaloneActionSet(IPropertyManager iPropertyManager, TextArea textArea, URL url)
		{
			super(null, url);
			this.iPropertyManager = iPropertyManager;
			this.textArea = textArea;
		}

		@Override
		protected JEditBeanShellAction[] getArray(int size)
		{
			return new JEditBeanShellAction[size];
		}

		@Override
		protected String getProperty(String name)
		{
			return iPropertyManager.getProperty(name);
		}

		public AbstractInputHandler getInputHandler()
		{
			return textArea.getInputHandler();
		}

		@Override
		protected JEditBeanShellAction createBeanShellAction(String actionName,
								     String code,
								     String selected,
								     boolean noRepeat,
								     boolean noRecord,
								     boolean noRememberLast)
		{
			return new JEditBeanShellAction(actionName,code,selected,noRepeat,noRecord,noRememberLast);
		}
	} 

	
	public static void main(String[] args)
	{
		JFrame frame = new JFrame();
		TextArea text = createTextArea();
		Mode mode = new Mode("xml");
		mode.setProperty("file","modes/xml.xml");
		ModeProvider.instance.addMode(mode);
		text.getBuffer().setMode(mode);
		frame.getContentPane().add(text);
		frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		frame.pack();
		frame.setVisible(true);
	} 
}
