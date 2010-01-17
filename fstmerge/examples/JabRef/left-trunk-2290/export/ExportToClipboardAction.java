package net.sf.jabref.export;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.Transferable;
import java.io.*;
import java.util.HashSet;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.ListSelectionModel;

import net.sf.jabref.*;


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

        Map<String, IExportFormat> m = ExportFormats.getExportFormats();
        IExportFormat[] formats = new ExportFormat[m.size()];
        String[] array = new String[formats.length];
        
        int piv = 0;
		for (IExportFormat format : m.values()) {
			formats[piv] = format;
			array[piv] = format.getDisplayName();
			piv++;
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

        IExportFormat format = formats[list.getSelectedIndex()];

        
        File tmp = null;
        Reader reader = null;
        try {
            
            
            tmp = File.createTempFile("jabrefCb", ".tmp");
            tmp.deleteOnExit();
            BibtexEntry[] bes = panel.getSelectedEntries();
            HashSet<String> entries = new HashSet<String>(bes.length);
            for (BibtexEntry be : bes)
                entries.add(be.getId());
            
            
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
