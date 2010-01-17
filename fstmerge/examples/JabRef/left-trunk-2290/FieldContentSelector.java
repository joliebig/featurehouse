
package net.sf.jabref;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.*;

import com.jgoodies.forms.layout.Sizes;
import com.jgoodies.looks.Options;


public class FieldContentSelector extends JComponent {

	JComboBox comboBox;

	FieldEditor editor;

	MetaData metaData;

	JabRefFrame frame;

	Window owner;

	BasePanel panel;

	String delimiter;

	
	public FieldContentSelector(JabRefFrame jabRefFrame, final BasePanel panel,
		Window ownerFrameOrDialog, final FieldEditor editor, final MetaData metaData,
		final AbstractAction action, boolean horizontalLayout, String delimiter) {

		this.frame = jabRefFrame;
		this.editor = editor;
		this.metaData = metaData;
		this.panel = panel;
		this.owner = ownerFrameOrDialog;
		this.delimiter = delimiter;

		comboBox = new JComboBox() {
			public Dimension getPreferredSize() {
				Dimension parents = super.getPreferredSize();
				if (parents.width > GUIGlobals.MAX_CONTENT_SELECTOR_WIDTH)
					parents.width = GUIGlobals.MAX_CONTENT_SELECTOR_WIDTH;
				return parents;
			}
		};

		GridBagLayout gbl = new GridBagLayout();
		GridBagConstraints con = new GridBagConstraints();

		setLayout(gbl);

		

		comboBox.setMaximumRowCount(35);

		
		comboBox.putClientProperty(Options.COMBO_POPUP_PROTOTYPE_DISPLAY_VALUE_KEY,
			"The longest text in the combo popup menu. And even longer.");

		rebuildComboBox();

		con.gridwidth = horizontalLayout ? 3 : GridBagConstraints.REMAINDER;
		con.fill = GridBagConstraints.HORIZONTAL;
		con.weightx = 1;
		gbl.setConstraints(comboBox, con);

		comboBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				
				if (e.getActionCommand().equals("comboBoxChanged") && (e.getModifiers() == 0))
					return;

				
				
				if (comboBox.getSelectedIndex() == 0)
					return;

				String chosen = (String) comboBox.getSelectedItem();
				if (chosen == null || chosen.equals(""))
					return;

				
				

				
				
				

				
				if (!editor.getText().equals(""))
					editor.append(FieldContentSelector.this.delimiter);

				editor.append(chosen);

				comboBox.setSelectedIndex(0);

				
				if (action != null)
					action.actionPerformed(new ActionEvent(editor, 0, ""));

				
				editor.requestFocus();
			}
		});

		add(comboBox);

		if (horizontalLayout)
			add(Box.createHorizontalStrut(Sizes.dialogUnitXAsPixel(2, this)));

		JButton manage = new JButton(Globals.lang("Manage"));
		gbl.setConstraints(manage, con);
		add(manage);

		manage.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				
				ContentSelectorDialog2 csd = owner instanceof Frame ? new ContentSelectorDialog2(
					(Frame) owner, frame, panel, true, metaData, editor.getFieldName())
					: new ContentSelectorDialog2((Dialog) owner, frame, panel, true, metaData,
						editor.getFieldName());
				Util.placeDialog(csd, frame);

				
				
				csd.setVisible(true);

				
				rebuildComboBox();
			}
		});
	}

	void rebuildComboBox() {
		comboBox.removeAllItems();

		
		comboBox.addItem("");
		Vector<String> items = metaData.getData(Globals.SELECTOR_META_PREFIX + editor.getFieldName());
		if (items != null) {
			Iterator<String> i = items.iterator();
			while (i.hasNext())
				comboBox.addItem(i.next());
		}
	}

	

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
}
