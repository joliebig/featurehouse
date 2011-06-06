

package org.gjt.sp.jedit.menu;


import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.nio.charset.Charset;

import java.util.Arrays;
import java.util.Hashtable;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;

import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.View;



public class ReloadWithEncodingProvider implements ActionListener, DynamicMenuProvider
{
	private View view;

	
	public boolean updateEveryTime()
	{
		return false;
	} 

	
	public void update(JMenu menu)
	{
		view = GUIUtilities.getView(menu);

		
		JMenuItem auto = new JMenuItem(
			jEdit.getProperty("vfs.browser.commands.encoding.auto-detect"));
		auto.setActionCommand("auto-detect");
		auto.addActionListener(this);
		menu.add(auto);
		menu.addSeparator();

		
		String[] encodings = MiscUtilities.getEncodings(true);
		String systemEncoding = System.getProperty("file.encoding");

		if (Arrays.binarySearch(encodings, systemEncoding) < 0) {
			String[] tmp_a = new String[encodings.length + 1];
			System.arraycopy(encodings, 0, tmp_a, 0, encodings.length);
			tmp_a[encodings.length] = systemEncoding;
			encodings = tmp_a;
		}

		Arrays.sort(encodings);

		int maxItems = jEdit.getIntegerProperty("menu.spillover",20);
		for (int i = 0; i < encodings.length; i++)
		{
			JMenuItem mi = new JMenuItem(encodings[i]);
			mi.setActionCommand("encoding@" + encodings[i]);
			mi.addActionListener(this);
			if ((menu.getMenuComponentCount() >= maxItems) && (i < encodings.length))
			{
				JMenu newMenu = new JMenu(jEdit.getProperty("common.more"));
				menu.add(newMenu);
				menu = newMenu;
			}
			menu.add(mi);
		}

		menu.addSeparator();

		
		JMenuItem other = new JMenuItem(
			jEdit.getProperty("vfs.browser.other-encoding.label"));
		other.setActionCommand("other-encoding");
		other.addActionListener(this);
		menu.add(other);
	} 

	
	public void actionPerformed(ActionEvent ae)
	{
		JMenuItem mi = (JMenuItem) ae.getSource();
		String action = mi.getActionCommand();
		String encoding = null;
		Hashtable props = null;

		if (action.startsWith("encoding@"))
		{
			encoding = action.substring(9);
		}
		else if (action.equals("other-encoding"))
		{
			encoding = JOptionPane.showInputDialog(view,
				jEdit.getProperty("encoding-prompt.message"),
				jEdit.getProperty("encoding-prompt.title"),
				JOptionPane.QUESTION_MESSAGE);
			if (encoding == null)
				return;

			if (!Charset.isSupported(encoding))
			{
				String msg = jEdit.getProperty("reload-encoding.error",
						new Object[] { encoding });
				JOptionPane.showMessageDialog(view,
					msg,
					jEdit.getProperty("common.error"),
					JOptionPane.ERROR_MESSAGE);
				return;
			}
		}

		if (encoding != null)
		{
			props = new Hashtable();
			props.put(Buffer.ENCODING, encoding);
			
			
			props.put(Buffer.ENCODING_AUTODETECT, false);
		}

		String path = view.getBuffer().getPath();
		jEdit.closeBuffer(view, view.getBuffer());
		jEdit.openFile(view,null,path,false,props);
	} 
}

