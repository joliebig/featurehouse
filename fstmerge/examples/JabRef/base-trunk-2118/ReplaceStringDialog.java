
package net.sf.jabref;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import net.sf.jabref.undo.*;


class ReplaceStringDialog extends JDialog {

    JTextField
        fields = new JTextField("", 30),
        from = new JTextField("", 30),
        to = new JTextField("", 30);
    JLabel
        fl = new JLabel(Globals.lang("Search for")+":"),
        tl = new JLabel(Globals.lang("Replace with")+":");

    JButton
        ok = new JButton(Globals.lang("Ok")),
        cancel = new JButton(Globals.lang("Cancel"));
    JPanel
        settings = new JPanel(),
        main = new JPanel(),
        opt = new JPanel();
    JCheckBox
        selOnly = new JCheckBox(Globals.lang("Limit to selected entries"), false);
    JRadioButton
        allFi = new JRadioButton(Globals.lang("All fields"), true),
        field = new JRadioButton(Globals.lang("Limit to fields")+":", false);
    ButtonGroup bg = new ButtonGroup();
    private boolean ok_pressed = false;
    private JabRefFrame parent;
    String[] flds = null;
    String s1, s2;

    GridBagLayout gbl = new GridBagLayout();
    GridBagConstraints con = new GridBagConstraints();

    public ReplaceStringDialog(JabRefFrame parent_) {
        super(parent_, Globals.lang("Replace string"), true);
        parent = parent_;

        bg.add(allFi);
        bg.add(field);
        ActionListener okListener = new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    s1 = from.getText();
                    s2 = to.getText();
                    if (s1.equals(""))
                        return;
                    ok_pressed = true;
                    flds = Util.delimToStringArray(fields.getText().toLowerCase(), ";");
                    dispose();
                }
            };
        ok.addActionListener(okListener);
        to.addActionListener(okListener);
        fields.addActionListener(okListener);
        AbstractAction cancelAction = new AbstractAction() {
                public void actionPerformed(ActionEvent e) {
                    dispose();
                }
            };
        cancel.addActionListener(cancelAction);

        
        ActionMap am = settings.getActionMap();
        InputMap im = settings.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        im.put(parent.prefs.getKey("Close dialog"), "close");
        am.put("close", cancelAction);

        
        settings.setLayout(gbl);
        opt.setLayout(gbl);
        main.setLayout(gbl);

        settings.setBorder(BorderFactory.createTitledBorder
                       (BorderFactory.createEtchedBorder(),
                        Globals.lang("Replace string")));
        main.setBorder(BorderFactory.createTitledBorder
                       (BorderFactory.createEtchedBorder(),
                        Globals.lang("Strings")));
          

        
        
        
        con.fill = GridBagConstraints.HORIZONTAL;
        
        con.gridwidth = 2;
        con.weightx = 0;
        con.anchor = GridBagConstraints.WEST;
        con.gridy = 0;
        con.gridx = 0;
        con.insets = new Insets(3, 5, 3, 5);
        gbl.setConstraints(selOnly, con);
        settings.add(selOnly);
        con.gridy = 1;
        con.insets = new Insets(13, 5, 3, 5);
        gbl.setConstraints(allFi, con);
        settings.add(allFi);
        con.gridwidth = 1;
        con.gridy = 2;
        con.gridx = 0;
        con.insets = new Insets(3, 5, 3, 5);
        gbl.setConstraints(field, con);
        settings.add(field);
        con.gridx = 1;
        con.weightx = 1;
        
        gbl.setConstraints(fields, con);
        settings.add(fields);

        con.weightx = 0;
        con.gridx = 0;
        con.gridy = 0;
        gbl.setConstraints(fl, con);
        main.add(fl);
        con.gridy = 1;
        gbl.setConstraints(tl, con);
        main.add(tl);
        con.weightx = 1;
        con.gridx = 1;
        con.gridy = 0;
        gbl.setConstraints(from, con);
        main.add(from);
        con.gridy = 1;
        gbl.setConstraints(to, con);
        main.add(to);

               
        con.gridx = GridBagConstraints.RELATIVE;
        con.gridy = GridBagConstraints.RELATIVE;
        con.weightx = 1;
        con.gridwidth = 1;
        con.anchor = GridBagConstraints.EAST;
        con.fill = GridBagConstraints.NONE;
        gbl.setConstraints(ok, con);
        opt.add(ok);
        con.anchor = GridBagConstraints.WEST;
        con.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(cancel, con);
        opt.add(cancel);

        getContentPane().add(main, BorderLayout.NORTH);
        getContentPane().add(settings, BorderLayout.CENTER);
        getContentPane().add(opt, BorderLayout.SOUTH);

        pack();
        

        Util.placeDialog(this, parent);
    }

    public boolean okPressed() { return ok_pressed; }
    public boolean allFields() { return allFi.isSelected(); }
    public boolean selOnly() { return selOnly.isSelected(); }
    public String[] fields() { return Util.delimToStringArray(field.getText(), ";"); }

    
    public int replace(BibtexEntry be, NamedCompound ce) {
        int counter = 0;
        if (allFields()) {
            Object[] os = be.getAllFields();
            for (int i=0; i<os.length; i++) {
                String s = (String)os[i];
                if (!s.equals(BibtexFields.KEY_FIELD))
                    counter += replaceField(be, s, ce);
            }
        } else {
            for (int i=0; i<flds.length; i++) {
                if (!flds[i].equals(BibtexFields.KEY_FIELD))
                    counter += replaceField(be, flds[i], ce);
            }

        }
        return counter;
    }

    public int replaceField(BibtexEntry be, String field, NamedCompound ce) {
        Object o = be.getField(field);
        if (o == null) return 0;
        String txt = o.toString();
        StringBuffer sb = new StringBuffer();
        int ind = -1, piv = 0, counter = 0, len1 = s1.length();
        while ((ind=txt.indexOf(s1, piv)) >= 0) {
            counter++;
            sb.append(txt.substring(piv, ind)); 
            sb.append(s2);  
            piv = ind+len1;
        }
        sb.append(txt.substring(piv));
        String newStr = sb.toString();
        be.setField(field, newStr);
        ce.addEdit(new UndoableFieldChange(be, field, txt, newStr));
        return counter;
    }
}
