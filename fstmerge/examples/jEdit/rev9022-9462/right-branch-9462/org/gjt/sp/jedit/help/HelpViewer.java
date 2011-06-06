

package org.gjt.sp.jedit.help;


import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.html.*;
import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.io.*;
import java.net.*;
import org.gjt.sp.jedit.msg.PluginUpdate;
import org.gjt.sp.jedit.msg.PropertiesChanged;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;



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

		rightPanel.add(BorderLayout.CENTER,new JScrollPane(viewer));

		splitter = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
					  jEdit.getBooleanProperty("appearance.continuousLayout"),
					  tabs,
					  rightPanel);
		splitter.setBorder(null);


		getContentPane().add(BorderLayout.CENTER,splitter);

		historyModel.addHelpHistoryModelListener(this);
		historyUpdated();

		gotoURL(url,true, 0);

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

	
	
	public void gotoURL(String url, boolean addToHistory, int scrollPos)
	{
		
		
		String shortURL;
		if(MiscUtilities.isURL(url))
		{
			if(url.startsWith(baseURL))
			{
				shortURL = url.substring(baseURL.length());
				if(shortURL.startsWith("/"))
					shortURL = shortURL.substring(1);
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
				url = baseURL + url;
			else
				url = baseURL + '/' + url;
		}

		
		
		viewer.setCursor(Cursor.getDefaultCursor());

		try
		{
			URL _url = new URL(url);

			if(!_url.equals(viewer.getPage()))
				title.setText(jEdit.getProperty("helpviewer.loading"));
			else
			{
				
			}

			viewer.setPage(_url);
			if(addToHistory)
				historyModel.addToHistory(url);
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
			toc.selectNode(shortURL);
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
	
	

	
	private String baseURL;
	private String shortURL;
	private HistoryButton back;
	private HistoryButton forward;
	private JEditorPane viewer;
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
			String url = evt.getActionCommand();
			if (url.length() != 0)
			{
				gotoURL(url,false,0);
				return;
			}
				
			if(source == back)
			{
				url = historyModel.back();
				if(url == null)
					getToolkit().beep();
				else
				{
					gotoURL(url,false,0);
				}
			}
			else if(source == forward)
			{
				url = historyModel.forward();
				if(url == null)
					getToolkit().beep();
				else
				{
					gotoURL(url,false,0);
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
						gotoURL(url.toString(),true,0);
				}
			}
			else if (evt.getEventType() == HyperlinkEvent.EventType.ENTERED) {
				viewer.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
			}
			else if (evt.getEventType() == HyperlinkEvent.EventType.EXITED) {
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

	public Component getComponent()
	{
		return getRootPane();
	}

	
}
