

package org.gjt.sp.jedit.pluginmgr;


import java.awt.BorderLayout;

import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.pluginmgr.ManagePanel.Entry;



class PluginDetailPanel extends JPanel
{
	private final JEditorPane pluginDetail;
	private final JLabel title;
	
	
	private Entry entry;
	
	
	PluginDetailPanel()
	{
		setLayout(new BorderLayout());
		pluginDetail = new JEditorPane();
		pluginDetail.setEditable(false);
		pluginDetail.setContentType("text/html");
		pluginDetail.setBackground(jEdit.getColorProperty("view.bgColor"));
		pluginDetail.setForeground(jEdit.getColorProperty("view.fgColor"));
		pluginDetail.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, true);
		title = new JLabel();
		add(title, BorderLayout.NORTH);
		JScrollPane scroll = new JScrollPane(pluginDetail);
		scroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		add(scroll);
	} 

	
	void setPlugin(Entry entry)
	{
		if (entry != this.entry)
		{
			if (entry.status.equals(Entry.LOADED))
			{
				title.setText("<html><b>"+entry.name+"</b></html>");
				StringBuilder builder = new StringBuilder();
				
				builder.append("<b>Version</b>: ").append(entry.version).append("<br/>");
				builder.append("<b>Author</b>: ").append(entry.author).append("<br/>");
				if (entry.description != null)
				{
					builder.append("<br/>").append(entry.description);
				}
				pluginDetail.setText(builder.toString());
			}
			else
			{
				title.setText("<html><b>"+entry.jar+"</b></html>");
				pluginDetail.setText(null);
			}
			this.entry = entry;
		}
	} 

}
