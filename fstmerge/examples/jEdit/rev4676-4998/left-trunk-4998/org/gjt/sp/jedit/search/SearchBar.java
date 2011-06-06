

package org.gjt.sp.jedit.search;


import java.awt.event.*;
import java.awt.*;
import javax.swing.event.*;
import javax.swing.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.util.Log;



public class SearchBar extends JPanel
{
	
	public SearchBar(final View view, boolean temp)
	{
		setLayout(new BoxLayout(this,BoxLayout.X_AXIS));

		this.view = view;

		add(Box.createHorizontalStrut(2));

		JLabel label = new JLabel(jEdit.getProperty("view.search.find"));
		add(label);
		add(Box.createHorizontalStrut(12));
		add(find = new HistoryTextField("find"));
		find.setSelectAllOnFocus(true);
		Dimension max = find.getPreferredSize();
		max.width = Integer.MAX_VALUE;
		find.setMaximumSize(max);
		ActionHandler actionHandler = new ActionHandler();
		find.addKeyListener(new KeyHandler());
		find.addActionListener(actionHandler);
		find.getDocument().addDocumentListener(new DocumentHandler());

		Insets margin = new Insets(1,1,1,1);

		add(Box.createHorizontalStrut(12));
		add(ignoreCase = new JCheckBox(jEdit.getProperty(
			"search.case")));
		ignoreCase.addActionListener(actionHandler);
		ignoreCase.setMargin(margin);
		ignoreCase.setRequestFocusEnabled(false);
		add(Box.createHorizontalStrut(2));
		add(regexp = new JCheckBox(jEdit.getProperty(
			"search.regexp")));
		regexp.addActionListener(actionHandler);
		regexp.setMargin(margin);
		regexp.setRequestFocusEnabled(false);
		add(Box.createHorizontalStrut(2));
		add(hyperSearch = new JCheckBox(jEdit.getProperty(
			"search.hypersearch")));
		hyperSearch.addActionListener(actionHandler);
		hyperSearch.setMargin(margin);
		hyperSearch.setRequestFocusEnabled(false);

		update();

		
		timer = new Timer(0,new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				if(!incrementalSearch(searchStart,searchReverse))
				{
					if(!incrementalSearch(
						(searchReverse
						? view.getBuffer().getLength()
						: 0),searchReverse))
					{
						
						view.getStatus().setMessageAndClear(
							jEdit.getProperty(
							"view.status.search-not-found"));
					}
				}
			}
		}); 

		
		this.temp = temp;

		propertiesChanged();
	} 

	
	public HistoryTextField getField()
	{
		return find;
	} 

	
	public void setHyperSearch(boolean hyperSearch)
	{
		jEdit.setBooleanProperty("view.search.hypersearch.toggle",hyperSearch);
		this.hyperSearch.setSelected(hyperSearch);
	} 

	
	public void update()
	{
		ignoreCase.setSelected(SearchAndReplace.getIgnoreCase());
		regexp.setSelected(SearchAndReplace.getRegexp());
		hyperSearch.setSelected(jEdit.getBooleanProperty(
			"view.search.hypersearch.toggle"));
	} 

	
	public void propertiesChanged()
	{
		if(temp)
		{
			if(close == null)
			{
				close = new RolloverButton(GUIUtilities.loadIcon("closebox.gif"));
				close.addActionListener(new ActionHandler());
				close.setToolTipText(jEdit.getProperty(
					"view.search.close-tooltip"));
			}
			add(close);
		}
		else if(close != null)
			remove(close);
	} 

	

	
	private View view;
	private HistoryTextField find;
	private JCheckBox ignoreCase, regexp, hyperSearch;
	private Timer timer;

	
	private RolloverButton close;

	private int searchStart;
	private boolean searchReverse;
	private boolean temp;
	

	
	private void find(boolean reverse)
	{
		timer.stop();

		String text = find.getText();
		
		if(text.length() == 0)
		{
			jEdit.setBooleanProperty("search.hypersearch.toggle",
				hyperSearch.isSelected());
			SearchDialog.showSearchDialog(view,null,SearchDialog.CURRENT_BUFFER);
		} 
		
		else if(hyperSearch.isSelected())
		{
			if(temp)
			{
				view.removeToolBar(SearchBar.this);
			}
                        else
				find.setText(null);

			SearchAndReplace.setSearchString(text);
			SearchAndReplace.setSearchFileSet(new CurrentBufferSet());
			SearchAndReplace.hyperSearch(view);
		} 
		
		else
		{
			if(reverse && SearchAndReplace.getRegexp())
			{
				GUIUtilities.error(view,"regexp-reverse",null);
				return;
			}

			
			
			int start;
			JEditTextArea textArea = view.getTextArea();
			Selection s = textArea.getSelectionAtOffset(
				textArea.getCaretPosition());
			if(s == null)
				start = textArea.getCaretPosition();
			else if(reverse)
				start = s.getStart();
			else
				start = s.getEnd();

			if(!incrementalSearch(start,reverse))
			{
				
				
				if(!incrementalSearch(reverse
					? view.getBuffer().getLength()
					: 0,reverse))
				{
					
					view.getStatus().setMessageAndClear(
						jEdit.getProperty(
						"view.status.search-not-found"));
				}
				else
				{
					
					view.getStatus().setMessageAndClear(
						jEdit.getProperty("view.status.auto-wrap"));
					
					if(jEdit.getBooleanProperty("search.beepOnSearchAutoWrap"))
					{
						getToolkit().beep();
					}
				}
			}
		} 
	} 

	
	private boolean incrementalSearch(int start, boolean reverse)
	{
		
		SearchAndReplace.setSearchFileSet(new CurrentBufferSet());
		SearchAndReplace.setSearchString(find.getText());
		SearchAndReplace.setReverseSearch(reverse);

		try
		{
			if(SearchAndReplace.find(view,view.getBuffer(),start,false,reverse))
				return true;
		}
		catch(Exception e)
		{
			Log.log(Log.DEBUG,this,e);

			
			
			
			return true;
		}

		return false;
	} 

	
	private void timerIncrementalSearch(int start, boolean reverse)
	{
		this.searchStart = start;
		this.searchReverse = reverse;

		timer.stop();
		timer.setRepeats(false);
		timer.setInitialDelay(150);
		timer.start();
	} 

	

	

	
	class ActionHandler implements ActionListener
	{
		
		public void actionPerformed(ActionEvent evt)
		{
			Object source = evt.getSource();
			if(source == find)
				find(false);
			else if(source == hyperSearch)
			{
				jEdit.setBooleanProperty("view.search.hypersearch.toggle",
					hyperSearch.isSelected());
				update();
			}
			else if(source == ignoreCase)
			{
				SearchAndReplace.setIgnoreCase(ignoreCase
					.isSelected());
			}
			else if(source == regexp)
			{
				SearchAndReplace.setRegexp(regexp
					.isSelected());
			}
			else if(source == close)
			{
				view.removeToolBar(SearchBar.this);
				view.getEditPane().focusOnTextArea();
			}
		} 
	} 

	
	class DocumentHandler implements DocumentListener
	{
		
		public void insertUpdate(DocumentEvent evt)
		{
			
			
			
			if(!hyperSearch.isSelected())
			{
				int start;
				JEditTextArea textArea = view.getTextArea();
				Selection s = textArea.getSelectionAtOffset(
					textArea.getCaretPosition());
				if(s == null)
					start = textArea.getCaretPosition();
				else
					start = s.getStart();

				timerIncrementalSearch(start,false);
			}
		} 

		
		public void removeUpdate(DocumentEvent evt)
		{
			
			if(!hyperSearch.isSelected())
			{
				String text = find.getText();
				if(text.length() != 0)
				{
					
					
					
					
					if(regexp.isSelected())
					{
						
						
						
						timerIncrementalSearch(0,false);
					}
					else
					{
						int start;
						JEditTextArea textArea = view.getTextArea();
						Selection s = textArea.getSelectionAtOffset(
							textArea.getCaretPosition());
						if(s == null)
							start = textArea.getCaretPosition();
						else
							start = s.getStart();
						timerIncrementalSearch(start,true);
					}
				}
			}
		} 

		
		public void changedUpdate(DocumentEvent evt) {}
		
	} 

	
	class KeyHandler extends KeyAdapter
	{
		public void keyPressed(KeyEvent evt)
		{
			switch(evt.getKeyCode())
			{
			case KeyEvent.VK_LEFT:
			case KeyEvent.VK_RIGHT:
				if(!hyperSearch.isSelected())
				{
					if(temp)
					{
						view.removeToolBar(SearchBar.this);
					}

					evt.consume();
					view.getEditPane().focusOnTextArea();
					view.getEditPane().getTextArea()
						.processKeyEvent(evt);
				}
				break;
			case KeyEvent.VK_ESCAPE:
				if(temp)
				{
					view.removeToolBar(SearchBar.this);
				}
				evt.consume();
				view.getEditPane().focusOnTextArea();
				break;
			case KeyEvent.VK_ENTER:
				if(evt.isShiftDown())
				{
					evt.consume();
					find(true);
				}
				break;
			}
		}
	} 

	
}
