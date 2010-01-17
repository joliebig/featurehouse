package net.sf.jabref.gui;

import com.jgoodies.forms.builder.DefaultFormBuilder;
import com.jgoodies.forms.builder.ButtonBarBuilder;
import com.jgoodies.forms.layout.FormLayout;

import javax.swing.*;

import net.sf.jabref.*;
import net.sf.jabref.external.ExternalFileType;
import net.sf.jabref.external.ConfirmCloseFileListEntryEditor;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;


public class FileListEntryEditor {

    JDialog diag;
    JTextField link = new JTextField(), description = new JTextField();
    JButton ok = new JButton(Globals.lang("Ok")),
            cancel = new JButton(Globals.lang("Cancel"));
    JComboBox types;
    JProgressBar prog = new JProgressBar(JProgressBar.HORIZONTAL);
    JLabel downloadLabel = new JLabel(Globals.lang("Downloading..."));
    ConfirmCloseFileListEntryEditor externalConfirm = null;

    private FileListEntry entry;
    private MetaData metaData;
    private boolean okPressed = false;

    public FileListEntryEditor(JFrame parent, FileListEntry entry, boolean showProgressBar,
                               MetaData metaData) {
        this.entry = entry;
        this.metaData = metaData;

        types = new JComboBox(Globals.prefs.getExternalFileTypeSelection());
        types.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent itemEvent) {
                ok.setEnabled(types.getSelectedItem() != null);
            }
        });
        DefaultFormBuilder builder = new DefaultFormBuilder(new FormLayout
                ("left:pref, 4dlu, fill:150dlu, 4dlu, fill:pref", ""));
        builder.append(Globals.lang("Link"));
        builder.append(link);
        BrowseListener browse = new BrowseListener(parent, link);
        JButton browseBut = new JButton(Globals.lang("Browse"));
        browseBut.addActionListener(browse);
        builder.append(browseBut);
        builder.nextLine();
        builder.append(Globals.lang("Description"));
        builder.append(description);
        builder.getPanel().setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
        builder.nextLine();
        builder.append(Globals.lang("File type"));
        builder.append(types);
        if (showProgressBar) {
            builder.nextLine();
            builder.append(downloadLabel);
            builder.append(prog);
        }
        
        ButtonBarBuilder bb = new ButtonBarBuilder();
        bb.addGlue();
        bb.addGridded(ok);
        bb.addGridded(cancel);
        bb.addGlue();

        ok.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                
                if (externalConfirm != null) {
                    
                    FileListEntry testEntry = new FileListEntry("", "", null);
                    storeSettings(testEntry);
                    if (!externalConfirm.confirmClose(testEntry))
                        return;
                }
                diag.dispose();
                storeSettings(FileListEntryEditor.this.entry);
                okPressed = true;
            }
        });
        cancel.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                diag.dispose();
            }
        });
        link.addFocusListener(new FocusListener() {
            public void focusGained(FocusEvent e) {
            }
            public void focusLost(FocusEvent e) {
                if ((types.getSelectedIndex() == -1) &&
                        (link.getText().trim().length() > 0)) {
                    
                    String theLink = link.getText().trim();
                    int index = theLink.indexOf('.');
                    if ((index >= 0) && (index < theLink.length()-1)) {

                        ExternalFileType type = Globals.prefs.getExternalFileTypeByExt
                                (theLink.substring(index+1));
                        if (type != null)
                            types.setSelectedItem(type);
                            
                    }
                }
            }
        });

        diag = new JDialog(parent, Globals.lang("Edit file link"), true);
        diag.getContentPane().add(builder.getPanel(), BorderLayout.CENTER);
        diag.getContentPane().add(bb.getPanel(), BorderLayout.SOUTH);
        diag.pack();
        Util.placeDialog(diag, parent);

        setValues(entry);
    }

    public void setExternalConfirm(ConfirmCloseFileListEntryEditor eC) {
        this.externalConfirm = eC;
    }

    public void setOkEnabled(boolean enabled) {
        ok.setEnabled(enabled);
    }

    public JProgressBar getProgressBar() {
        return prog;
    }

    public JLabel getProgressBarLabel() {
        return downloadLabel;
    }

    public void setEntry(FileListEntry entry) {
        this.entry = entry;
        setValues(entry);
    }

    public void setVisible(boolean visible) {
        if (visible)
            okPressed = false;
        diag.setVisible(visible);
    }

    public void setValues(FileListEntry entry) {
        description.setText(entry.getDescription());
        link.setText(entry.getLink());
        types.setSelectedItem(entry.getType());
    }

    public void storeSettings(FileListEntry entry) {
        entry.setDescription(description.getText().trim());
        entry.setLink(link.getText().trim());
        entry.setType((ExternalFileType)types.getSelectedItem());
    }

    public boolean okPressed() {
        return okPressed;
    }

    class BrowseListener implements ActionListener {
        private JFrame parent;
        private JTextField comp;

        public BrowseListener(JFrame parent, JTextField comp) {
            this.parent = parent;
            this.comp = comp;
        }

        public void actionPerformed(ActionEvent e) {
            File initial = new File(comp.getText().trim());
            if (comp.getText().trim().length() == 0) {
                
                initial = new File(Globals.prefs.get("fileWorkingDirectory"));
            }
            String chosen = Globals.getNewFile(parent, initial, Globals.NONE,
                JFileChooser.OPEN_DIALOG, false);
            if (chosen != null) {
                File newFile = new File(chosen);
                
                Globals.prefs.put("fileWorkingDirectory", newFile.getParent());

                
                ArrayList<File> dirs = new ArrayList<File>();
                dirs.add(new File(metaData.getFileDirectory(GUIGlobals.FILE_FIELD)));
                if (dirs.size() > 0) {
                    newFile = FileListEditor.relativizePath(newFile, dirs);
                }

                comp.setText(newFile.getPath());
                comp.requestFocus();
            }
        }
    }
}
