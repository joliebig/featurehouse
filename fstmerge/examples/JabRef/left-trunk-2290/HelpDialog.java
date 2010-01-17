
package net.sf.jabref;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;

import javax.swing.*;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;


public class HelpDialog extends JDialog implements HyperlinkListener {

	private JabRefFrame frame;

	private HelpContent content;

	private BackAction back = new BackAction();

	private ForwardAction forward = new ForwardAction();

	private ContentsAction contents = new ContentsAction();

	
	public HelpDialog(JabRefFrame bf) {
		super(bf, Globals.lang("JabRef help"), false);
		frame = bf;
		content = new HelpContent(bf.prefs);
		content.addHyperlinkListener(this);
		setSize(GUIGlobals.helpSize);

		JToolBar tlb = new JToolBar();
		tlb.add(back);
		tlb.add(forward);
		tlb.addSeparator();
		tlb.add(contents);
		tlb.setFloatable(false);

		
		InputMap im = tlb.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
		ActionMap am = tlb.getActionMap();
		im.put(bf.prefs.getKey("Close dialog"), "close");
		am.put("close", new CloseAction());
		im.put(bf.prefs.getKey("Back, help dialog"), "left");
		am.put("left", back);
		im.put(bf.prefs.getKey("Forward, help dialog"), "right");
		am.put("right", forward);

		
		im = content.getInputMap(JComponent.WHEN_FOCUSED);
		am = content.getActionMap();
		im.put(bf.prefs.getKey("Back, help dialog"), "left");
		am.put("left", back);
		im.put(bf.prefs.getKey("Forward, help dialog"), "right");
		am.put("right", forward);

		getContentPane().add(tlb, BorderLayout.NORTH);
		getContentPane().add(content.getPane());
		forward.setEnabled(false);
		back.setEnabled(false);
	}

	public void showPage(String url) {
		if (!isVisible()) {
			Util.placeDialog(this, frame);
			content.reset();
			back.setEnabled(false);
			setVisible(true);
		} else {
			back.setEnabled(true);
		}
		forward.setEnabled(false);
		content.setPage(url);
		content.requestFocus();
	}

	public void hyperlinkUpdate(HyperlinkEvent e) {
		if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
			content.setPage(e.getURL());
			back.setEnabled(true);
			forward.setEnabled(false);
		}
	}

	class CloseAction extends AbstractAction {
		public CloseAction() {
			super(Globals.lang("Close"));
			
			putValue(SHORT_DESCRIPTION, Globals.lang("Close the help window"));
		}

		public void actionPerformed(ActionEvent e) {
			dispose();
		}
	}

	class BackAction extends AbstractAction {
		public BackAction() {
			super("Back", GUIGlobals.getImage("left"));
			
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(content.back());
			forward.setEnabled(true);
		}
	}

	class ForwardAction extends AbstractAction {
		public ForwardAction() {
			super("Forward", GUIGlobals.getImage("right"));
		}

		public void actionPerformed(ActionEvent e) {
			setEnabled(content.forward());
			back.setEnabled(true);
		}
	}

	class ContentsAction extends AbstractAction {
		public ContentsAction() {
			super("Contents", GUIGlobals.getImage("helpContents"));
		}

		public void actionPerformed(ActionEvent e) {
			content.setPage(GUIGlobals.helpContents);
			back.setEnabled(true);
		}
	}
}
