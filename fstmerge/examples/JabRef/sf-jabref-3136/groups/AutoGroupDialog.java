
package net.sf.jabref.groups;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;

import javax.swing.*;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;

import net.sf.jabref.BasePanel;
import net.sf.jabref.Globals;
import net.sf.jabref.JabRefFrame;
import net.sf.jabref.Util;
import net.sf.jabref.undo.NamedCompound;
import com.jgoodies.forms.builder.DefaultFormBuilder;
import com.jgoodies.forms.builder.ButtonBarBuilder;
import com.jgoodies.forms.layout.FormLayout;


class AutoGroupDialog extends JDialog implements CaretListener {
    JTextField remove = new JTextField(60), field = new JTextField(60),
            deliminator = new JTextField(60);
    JLabel nf = new JLabel(Globals.lang("Field to group by") + ":"),
            nr = new JLabel(Globals.lang("Characters to ignore") + ":");
    JRadioButton
        keywords = new JRadioButton(Globals.lang("Generate groups from keywords in a BibTeX field")),
        authors = new JRadioButton(Globals.lang("Generate groups for author last names")),
        editors = new JRadioButton(Globals.lang("Generate groups for editor last names"));
    JCheckBox nd = new JCheckBox(Globals.lang(
    		"Use the following delimiter character(s)")
            + ":"); 
    JButton ok = new JButton(Globals.lang("Ok")), cancel = new JButton(Globals
            .lang("Cancel"));
    JPanel main = new JPanel(), opt = new JPanel();
    private boolean ok_pressed = false;
    private GroupTreeNode m_groupsRoot;
    private JabRefFrame frame;
    private BasePanel panel;
    private GroupSelector gs;
    private String oldRemove, oldField;
    GridBagLayout gbl = new GridBagLayout();
    GridBagConstraints con = new GridBagConstraints();

    
    public AutoGroupDialog(JabRefFrame jabrefFrame, BasePanel basePanel,
            GroupSelector groupSelector, GroupTreeNode groupsRoot,
            String defaultField, String defaultRemove, String defaultDeliminator) {
        super(jabrefFrame, Globals.lang("Automatically create groups"), true);
        frame = jabrefFrame;
        gs = groupSelector;
        panel = basePanel;
        m_groupsRoot = groupsRoot;
        field.setText(defaultField);
        remove.setText(defaultRemove);
        deliminator.setText(defaultDeliminator);
        nd.setSelected(true);
        ActionListener okListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                ok_pressed = true;
                dispose();

                GroupTreeNode autoGroupsRoot = new GroupTreeNode(
                        new ExplicitGroup(Globals.lang("Automatically created groups"),
                        		AbstractGroup.INCLUDING));
                Set<String> hs = null;
                String field = field();
                if (keywords.isSelected()) {
                    if (nd.isSelected()) {
                        hs = Util
                                .findDeliminatedWordsInField(panel.getDatabase(),
                                        field().toLowerCase().trim(), deliminator
                                                .getText());
                    } else {
                        hs = Util.findAllWordsInField(panel.getDatabase(),
                                field().toLowerCase().trim(), remove());

                    }
                }
                else if (authors.isSelected()) {
                    List<String> fields = new ArrayList<String>(2);
                    fields.add("author");
                    hs = Util.findAuthorLastNames(panel.getDatabase(), fields);
                    field = "author";
                }
                else if (editors.isSelected()) {
                    List<String> fields = new ArrayList<String>(2);
                    fields.add("editor");
                    hs = Util.findAuthorLastNames(panel.getDatabase(), fields);
                    field = "editor";
                }

                for (String keyword : hs){
                    KeywordGroup group = new KeywordGroup(keyword, field,
                            keyword, false, false, AbstractGroup.INDEPENDENT);
                    autoGroupsRoot.add(new GroupTreeNode(group));
                }

                m_groupsRoot.add(autoGroupsRoot);
                NamedCompound ce = new NamedCompound(Globals
                        .lang("Autogenerate groups"));
                UndoableAddOrRemoveGroup undo = new UndoableAddOrRemoveGroup(
                        gs, m_groupsRoot, autoGroupsRoot,
                        UndoableAddOrRemoveGroup.ADD_NODE);
                undo.setRevalidate(true);
                ce.addEdit(undo);

                panel.markBaseChanged(); 
                gs.revalidateGroups();
                frame.output(Globals.lang("Created groups."));
                ce.end();
                panel.undoManager.addEdit(ce);
            }
        };
        remove.addActionListener(okListener);
        field.addActionListener(okListener);
        field.addCaretListener(this);
        AbstractAction cancelAction = new AbstractAction() {
            public void actionPerformed(ActionEvent e) {
                dispose();
            }
        };
        cancel.addActionListener(cancelAction);
        ok.addActionListener(okListener);
        
        ActionMap am = main.getActionMap();
        InputMap im = main.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        im.put(frame.prefs().getKey("Close dialog"), "close");
        am.put("close", cancelAction);

        ButtonGroup bg = new ButtonGroup();
        bg.add(keywords);
        bg.add(authors);
        bg.add(editors);
        keywords.setSelected(true);
        DefaultFormBuilder b = new DefaultFormBuilder(new FormLayout
                ("left:20dlu, 4dlu, left:pref, 4dlu, fill:60dlu, 4dlu, fill:0dlu", ""), main);
        b.append(keywords, 5);
        b.nextLine();
        b.append(new JPanel());
        b.append(Globals.lang("Field to group by")+":");
        b.append(field);
        b.nextLine();
        b.append(new JPanel());
        b.append(Globals.lang("Characters to ignore")+":");
        b.append(remove);
        b.nextLine();
        b.append(new JPanel());
        b.append(nd);
        b.append(deliminator);
        b.nextLine();
        b.append(authors, 5);
        b.nextLine();
        b.append(editors, 5);
        b.nextLine();
        
        ButtonBarBuilder bb = new ButtonBarBuilder(opt);
        bb.addGlue();
        bb.addGridded(ok);
        bb.addGridded(cancel);
        bb.addGlue();


        
        
        main.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
        opt.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
        getContentPane().add(main, BorderLayout.CENTER);
        getContentPane().add(opt, BorderLayout.SOUTH);
        
        updateComponents();
        pack();
        Util.placeDialog(this, frame);
    }

    public boolean okPressed() {
        return ok_pressed;
    }

    public String oldField() {
        return oldField;
    }

    public String oldRemove() {
        return oldRemove;
    }

    public String field() {
        return field.getText();
    }

    public String remove() {
        return remove.getText();
    }

    public void caretUpdate(CaretEvent e) {
        updateComponents();
    }
    
    protected void updateComponents() {
        String groupField = field.getText().trim();
        ok.setEnabled(groupField.matches("\\w+"));
    }
}
