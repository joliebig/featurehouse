

package net.sf.jabref;

import com.jgoodies.forms.builder.ButtonBarBuilder;

import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.util.*;

public class EntryTypeDialog extends JDialog implements ActionListener {

    

    BibtexEntryType type = null;
    CancelAction cancelAction = new CancelAction();
    private final int COLNUM = 3;

    class TypeButton extends JButton implements Comparable {
	BibtexEntryType type;
	public TypeButton(String label, BibtexEntryType type_) {
	    super(label);
	    type = type_;
	}
	public int compareTo(Object o) {
	    if (! (o instanceof TypeButton))
		throw new ClassCastException();
	    return type.getName().compareTo(((TypeButton)o).type.getName());
	}
    }

    public EntryTypeDialog(JabRefFrame baseFrame_) {
	super(baseFrame_, true); 


	setTitle(Globals.lang("Select entry type"));

	addWindowListener(new WindowAdapter() {
		public void windowClosing(WindowEvent e) {
		    cancelAction.actionPerformed(null);
		}
	    });

	getContentPane().setLayout(new BorderLayout());
	JPanel pan = new JPanel();
	getContentPane().add(pan, BorderLayout.CENTER);
	JPanel buttons = new JPanel();
	JButton 
	    cancel = new JButton(Globals.lang("Cancel"));
	
	cancel.addActionListener(this);

    
	cancel.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
	    .put(baseFrame_.prefs.getKey("Close dialog"), "close");
	cancel.getActionMap().put("close", cancelAction);

	
    ButtonBarBuilder bb = new ButtonBarBuilder(buttons);
    
    bb.addGlue();
    bb.addGridded(cancel);
    bb.addGlue();

    getContentPane().add(buttons, BorderLayout.SOUTH);
	GridBagLayout gbl = new GridBagLayout();
	pan.setLayout(gbl);
	GridBagConstraints con = new GridBagConstraints();
	con.anchor = GridBagConstraints.WEST;
	con.fill = GridBagConstraints.HORIZONTAL;
	con.insets = new Insets(4, 4, 4, 4);
	int col = 0;

	Iterator iter = BibtexEntryType.ALL_TYPES.keySet().iterator();
	while (iter.hasNext()) {
	    BibtexEntryType tp = BibtexEntryType.getType((String)iter.next());
            
	    TypeButton b = new TypeButton(Util.nCase(tp.getName()), tp);
	    b.setAlignmentX(SwingConstants.LEFT);
	    b.addActionListener(this);
	    
	    col++;
	    if (col == COLNUM) {
		col = 0;
		con.gridwidth = GridBagConstraints.REMAINDER;
	    } else
		con.gridwidth = 1;
	    gbl.setConstraints(b, con);
	    pan.add(b);
	}
	pan.setBorder(BorderFactory.createTitledBorder
		      (BorderFactory.createEtchedBorder(),
		       Globals.lang("Entry types")));
	
	
	pack();
	setResizable(false);
    }

    public void actionPerformed(ActionEvent e) {
	if (e.getSource() instanceof TypeButton) {
	    type = ((TypeButton)e.getSource()).type;
	}
	dispose();
    }

    public BibtexEntryType getChoice() {
	
	return type;
    }

    class CancelAction extends AbstractAction {
	public CancelAction() {
	    super("Cancel");
	    
	    
	    
	}
	public void actionPerformed(ActionEvent e) {
	    dispose();
	}
    }


}
