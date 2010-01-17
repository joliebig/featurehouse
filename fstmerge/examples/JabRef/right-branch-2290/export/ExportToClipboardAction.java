package net.sf.jabref.export;

import net.sf.jabref.*;
import net.sf.jabref.gui.MainTable;

import javax.swing.*;
import java.util.*;
import java.io.*;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.Transferable;
import java.awt.*;


public class ExportToClipboardAction extends AbstractWorker {
    String message = null;
    private JabRefFrame frame;
    private BibtexDatabase database;

    public ExportToClipboardAction(JabRefFrame frame, BibtexDatabase database) {
        this.frame = frame;
        this.database = database;
    }
    public void run() {
        BasePanel panel = frame.basePanel();
        if (panel == null)
            return;
        if (panel.getSelectedEntries().length == 0) {
            message = Globals.lang("No entries selected") + ".";
            getCallBack().update();
            return;
        }

        Map m = ExportFormats.getExportFormats();
        ExportFormat[] formats = new ExportFormat[m.size()];
        int piv=0;
        for (Iterator iterator = m.keySet().iterator(); iterator.hasNext();) {
            formats[piv++] = (ExportFormat)m.get(iterator.next());
        }
        
        

        
        
        String[] array = new String[formats.length];
        for (int i=0; i<formats.length; i++) {
            array[i] = formats[i].getDisplayName();
        }
        
        JList list = new JList(array);
        list.setBorder(BorderFactory.createEtchedBorder());
        list.setSelectionInterval(0, 0);
        list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        int answer = JOptionPane.showOptionDialog(frame, list, Globals.lang("Select format"),
            JOptionPane.YES_NO_OPTION,
            JOptionPane.QUESTION_MESSAGE, null,
            new String[]{Globals.lang("Ok"), Globals.lang("Cancel")},
            Globals.lang("Ok"));

        if (answer == JOptionPane.NO_OPTION)
            return;

        ExportFormat format = formats[list.getSelectedIndex()];

        
        File tmp = null;
        Reader reader = null;
        try {
            
            
            tmp = File.createTempFile("jabrefCb", ".tmp");
            tmp.deleteOnExit();
            BibtexEntry[] bes = panel.getSelectedEntries();
            HashSet entries = new HashSet(bes.length);
            for (int i = 0; i < bes.length; i++)
                entries.add(bes[i].getId());
            
            format.performExport(database, tmp.getPath(), panel.getEncoding(), entries);
            
            StringBuffer sb = new StringBuffer();
            reader = new InputStreamReader(new FileInputStream(tmp), panel.getEncoding());
            int s;
            while ((s = reader.read()) != -1) {
                sb.append((char)s);
            }
            ClipboardOwner owner = new ClipboardOwner() {
                public void lostOwnership(Clipboard clipboard, Transferable content) {
                }
            };
            
            RtfSelection rs = new RtfSelection(sb.toString());
            Toolkit.getDefaultToolkit().getSystemClipboard()
                    .setContents(rs, owner);
            message = Globals.lang("Entries exported to clipboard") + ": " + bes.length;

        } catch (Exception e) {
            e.printStackTrace();  
            message = Globals.lang("Error exporting to clipboard");
            return;
        } finally {
            
            if (tmp != null)
                tmp.delete();
            if (reader != null)
                try { reader.close(); } catch (IOException ex) { ex.printStackTrace(); }
        }

    }

    public void update() {
        frame.output(message);
    }

}
