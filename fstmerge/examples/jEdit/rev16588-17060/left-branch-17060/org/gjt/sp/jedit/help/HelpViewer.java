

package org.gjt.sp.jedit.help;


import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import java.io.File;
import java.io.IOException;

import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;

import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLFrameHyperlinkEvent;

import org.gjt.sp.jedit.EBComponent;
import org.gjt.sp.jedit.EBMessage;
import org.gjt.sp.jedit.EditBus;
import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.MiscUtilities;

import org.gjt.sp.jedit.msg.PluginUpdate;
import org.gjt.sp.jedit.msg.PropertiesChanged;

import org.gjt.sp.util.Log;

import static org.gjt.sp.jedit.help.HelpHistoryModel.HistoryEntry;



public class HelpViewer extends JFrame implements HelpViewerInterface, EBComponent, HelpHistoryModelListener
{
	
	
	public HelpViewer()
	{
		this("welcome.html");
	} 

	
	
	public HelpViewer(URL url)
	{
		this(url.toString());
	} 

	
	
	public HelpViewer(String url)
	{
		super(jEdit.getProperty("helpviewer.title"));

		setIconImage(GUIUtilities.getEditorIcon());

		try
		{
			baseURL = new File(MiscUtilities.constructPath(
				jEdit.getJEditHome(),"doc")).toURL().toString();
		}
		catch(MalformedURLException mu)
		{
			Log.log(Log.ERROR,this,mu);
			
		}

		ActionHandler actionListener = new ActionHandler();

		JTabbedPane tabs = new JTabbedPane();
		tabs.addTab(jEdit.getProperty("helpviewer.toc.label"),
			toc = new HelpTOCPanel(this));
		tabs.addTab(jEdit.getProperty("helpviewer.search.label"),
			new HelpSearchPanel(this));
		tabs.setMinimumSize(new Dimension(0,0));

		JPanel rightPanel = new JPanel(new BorderLayout());

		Box toolBar = new Box(BoxLayout.X_AXIS);
		

		toolBar.add(title = new JLabel());
		toolBar.add(Box.createGlue());
		historyModel = new HelpHistoryModel(25);
		back = new HistoryButton(HistoryButton.BACK,historyModel);
		back.addActionListener(actionListener);
		toolBar.add(back);
		forward = new HistoryButton(HistoryButton.FORWARD,historyModel);
		forward.addActionListener(actionListener);
		toolBar.add(forward);
		back.setPreferredSize(forward.getPreferredSize());
		rightPanel.add(BorderLayout.NORTH,toolBar);

		viewer = new JEditorPane();
		viewer.setEditable(false);
		viewer.addHyperlinkListener(new LinkHandler());
		viewer.setFont(new Font("Monospaced",Font.PLAIN,12));
		viewer.addPropertyChangeListener(new PropertyChangeHandler());
		viewer.addKeyListener(new KeyHandler());

		viewerScrollPane = new JScrollPane(viewer);

		rightPanel.add(BorderLayout.CENTER,viewerScrollPane);

		splitter = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
					  jEdit.getBooleanProperty("appearance.continuousLayout"),
					  tabs,
					  rightPanel);
		splitter.setBorder(null);


		getContentPane().add(BorderLayout.CENTER,splitter);

		historyModel.addHelpHistoryModelListener(this);
		historyUpdated();

		gotoURL(url,true,0);

		setDefaultCloseOperation(DISPOSE_ON_CLOSE);

		getRootPane().setPreferredSize(new Dimension(750,500));

		pack();
		GUIUtilities.loadGeometry(this,"helpviewer");
		GUIUtilities.addSizeSaver(this,"helpviewer");

		EditBus.addToBus(this);

		setVisible(true);

		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				splitter.setDividerLocation(jEdit.getIntegerProperty(
					"helpviewer.splitter",250));
				viewer.requestFocus();
			}
		});
	} 

	
	
	public void gotoURL(String url, boolean addToHistory, final int scrollPosition)
	{
		
		
		String shortURL;
		if (MiscUtilities.isURL(url))
		{
			if (url.startsWith(baseURL))
			{
				shortURL = url.substring(baseURL.length());
				if(shortURL.startsWith("/"))
				{
					shortURL = shortURL.substring(1);
				}
			}
			else
			{
				shortURL = url;
			}
		}
		else
		{
			shortURL = url;
			if(baseURL.endsWith("/"))
			{
				url = baseURL + url;
			}
			else
			{
				url = baseURL + '/' + url;
			}
		}

		
		
		viewer.setCursor(Cursor.getDefaultCursor());

		try
		{
			URL _url = new URL(url);

			if(!_url.equals(viewer.getPage()))
			{
				title.setText(jEdit.getProperty("helpviewer.loading"));
			}
			else
			{
				
			}

			historyModel.setCurrentScrollPosition(viewer.getPage(),getCurrentScrollPosition());
			viewer.setPage(_url);
			if (0 != scrollPosition)
			{
				SwingUtilities.invokeLater(new Runnable()
				{
					public void run()
					{
						viewerScrollPane.getVerticalScrollBar().setValue(scrollPosition);
					}
				});
			}
			if(addToHistory)
			{
				historyModel.addToHistory(url);
			}
		}
		catch(MalformedURLException mf)
		{
			Log.log(Log.ERROR,this,mf);
			String[] args = { url, mf.getMessage() };
			GUIUtilities.error(this,"badurl",args);
			return;
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,this,io);
			String[] args = { url, io.toString() };
			GUIUtilities.error(this,"read-error",args);
			return;
		}

		this.shortURL = shortURL;

		
		if(shortURL != null)
		{
			toc.selectNode(shortURL);
		}
		
		viewer.requestFocus();
	} 

	
	int getCurrentScrollPosition() {
		return viewerScrollPane.getVerticalScrollBar().getValue();
	} 

	
	URL getCurrentPage() {
		return viewer.getPage();
	} 

	
	public void dispose()
	{
		EditBus.removeFromBus(this);
		jEdit.setIntegerProperty("helpviewer.splitter",
			splitter.getDividerLocation());
		super.dispose();
	} 

	
	public void handleMessage(EBMessage msg)
	{
		if(msg instanceof PluginUpdate)
		{
			PluginUpdate pmsg = (PluginUpdate)msg;
			if(pmsg.getWhat() == PluginUpdate.LOADED
				|| pmsg.getWhat() == PluginUpdate.UNLOADED)
			{
				if(!pmsg.isExiting())
				{
					if(!queuedTOCReload)
						queueTOCReload();
					queuedTOCReload = true;
				}
			}
		}
		else if (msg instanceof PropertiesChanged)
		{
			GUIUtilities.initContinuousLayout(splitter);
		}
	} 

	
	public String getBaseURL()
	{
		return baseURL;
	} 

	
	public String getShortURL()
	{
		return shortURL;
	} 

	
	public void historyUpdated()
	{
		back.setEnabled(historyModel.hasPrevious());
		forward.setEnabled(historyModel.hasNext());
	} 

	
	public Component getComponent()
	{
		return getRootPane();
	} 

	

	
	private String baseURL;
	private String shortURL;
	private HistoryButton back;
	private HistoryButton forward;
	private JEditorPane viewer;
	private JScrollPane viewerScrollPane;
	private JLabel title;
	private JSplitPane splitter;
	private HelpHistoryModel historyModel;
	private HelpTOCPanel toc;
	private boolean queuedTOCReload;
	

	
	public void queueTOCReload()
	{
		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				queuedTOCReload = false;
				toc.load();
			}
		});
	} 

	

	

	
	class ActionHandler implements ActionListener
	{
		
		public void actionPerformed(ActionEvent evt)
		{
			Object source = evt.getSource();
			String actionCommand = evt.getActionCommand();
			int separatorPosition = actionCommand.lastIndexOf(':');
			String url;
			int scrollPosition;
			if (-1 == separatorPosition)
			{
				url = actionCommand;
				scrollPosition = 0;
			}
			else
			{
				url = actionCommand.substring(0,separatorPosition);
				scrollPosition = Integer.parseInt(actionCommand.substring(separatorPosition+1));
			}
			if (url.length() != 0)
			{
				gotoURL(url,false,scrollPosition);
				return;
			}

			if(source == back)
			{
				HistoryEntry entry = historyModel.back(HelpViewer.this);
				if(entry == null)
				{
					getToolkit().beep();
				}
				else
				{
					gotoURL(entry.url,false,entry.scrollPosition);
				}
			}
			else if(source == forward)
			{
				HistoryEntry entry = historyModel.forward(HelpViewer.this);
				if(entry == null)
				{
					getToolkit().beep();
				}
				else
				{
					gotoURL(entry.url,false,entry.scrollPosition);
				}
			}
		} 
	} 

	
	class LinkHandler implements HyperlinkListener
	{
		
		public void hyperlinkUpdate(HyperlinkEvent evt)
		{
			if(evt.getEventType() == HyperlinkEvent.EventType.ACTIVATED)
			{
				if(evt instanceof HTMLFrameHyperlinkEvent)
				{
					((HTMLDocument)viewer.getDocument())
						.processHTMLFrameHyperlinkEvent(
						(HTMLFrameHyperlinkEvent)evt);
					historyUpdated();
				}
				else
				{
					URL url = evt.getURL();
					if(url != null)
					{
						gotoURL(url.toString(),true,0);
					}
				}
			}
			else if (evt.getEventType() == HyperlinkEvent.EventType.ENTERED)
			{
				viewer.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
			}
			else if (evt.getEventType() == HyperlinkEvent.EventType.EXITED)
			{
				viewer.setCursor(Cursor.getDefaultCursor());
			}
		} 
	} 

	
	class PropertyChangeHandler implements PropertyChangeListener
	{
		public void propertyChange(PropertyChangeEvent evt)
		{
			if("page".equals(evt.getPropertyName()))
			{
				String titleStr = (String)viewer.getDocument()
					.getProperty("title");
				if(titleStr == null)
				{
					titleStr = MiscUtilities.getFileName(
						viewer.getPage().toString());
				}
				title.setText(titleStr);
				historyModel.updateTitle(viewer.getPage().toString(),
					titleStr);
			}
		}
	} 

	
	private class KeyHandler extends KeyAdapter
	{
		public void keyPressed(KeyEvent ke)
		{
			switch (ke.getKeyCode())
			{
			case KeyEvent.VK_UP:
				JScrollBar scrollBar = viewerScrollPane.getVerticalScrollBar();
				scrollBar.setValue(scrollBar.getValue()-scrollBar.getUnitIncrement(-1));
				ke.consume();
				break;
			case KeyEvent.VK_DOWN:
				scrollBar = viewerScrollPane.getVerticalScrollBar();
				scrollBar.setValue(scrollBar.getValue()+scrollBar.getUnitIncrement(1));
				ke.consume();
				break;
			case KeyEvent.VK_LEFT:
				scrollBar = viewerScrollPane.getHorizontalScrollBar();
				scrollBar.setValue(scrollBar.getValue()-scrollBar.getUnitIncrement(-1));
				ke.consume();
				break;
			case KeyEvent.VK_RIGHT:
				scrollBar = viewerScrollPane.getHorizontalScrollBar();
				scrollBar.setValue(scrollBar.getValue()+scrollBar.getUnitIncrement(1));
				ke.consume();
				break;
			}
		}
	} 

	
}
