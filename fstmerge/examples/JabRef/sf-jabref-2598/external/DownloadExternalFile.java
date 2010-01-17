package net.sf.jabref.external;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import net.sf.jabref.*;
import net.sf.jabref.gui.FileListEntry;
import net.sf.jabref.gui.FileListEntryEditor;
import net.sf.jabref.net.URLDownload;


public class DownloadExternalFile {
    private JabRefFrame frame;
    private MetaData metaData;
    private String bibtexKey;
    private FileListEntryEditor editor;
    private boolean downloadFinished = false;

    public DownloadExternalFile(JabRefFrame frame, MetaData metaData, String bibtexKey) {

        this.frame = frame;
        this.metaData = metaData;
        this.bibtexKey = bibtexKey;
    }

    
    public void download(final DownloadCallback callback) throws IOException {

        final String res = JOptionPane.showInputDialog(frame,
                Globals.lang("Enter URL to download"));

        if (res == null || res.trim().length() == 0)
            return;

        
        final File tmp = File.createTempFile("jabref_download", "tmp");
        tmp.deleteOnExit();
        (new Thread() {
            public void run() {

                try {

                    URL url = new URL(res);
                    URLDownload udl = new URLDownload(frame, url, tmp);
                    try {
                        udl.download();
                    } catch (IOException e2) {
                        JOptionPane.showMessageDialog(frame, Globals.lang("Invalid URL")+": "
                                + e2.getMessage(), Globals.lang("Download file"),
                                JOptionPane.ERROR_MESSAGE);
                        Globals.logger("Error while downloading " + url.toString());
                        return;
                    }

                    
                    SwingUtilities.invokeLater(new Runnable() {
                        public void run() {
                            downloadFinished();
                        }
                    });


                } catch (MalformedURLException e1) {
                    JOptionPane.showMessageDialog(frame, Globals.lang("Invalid URL"), Globals
                            .lang("Download file"), JOptionPane.ERROR_MESSAGE);
                }
            }
        }).start();

        
        String suffix = getSuffix(res);
        String suggestedName = bibtexKey != null ? getSuggestedFileName(res, suffix) : "";
        String fDirectory = getFileDirectory(res);
        if (fDirectory.trim().equals(""))
            fDirectory = null;
        final String directory = fDirectory;
        final String suggestDir = directory != null ? directory : System.getProperty("user.home");
        File file = new File(new File(suggestDir), suggestedName);
        FileListEntry entry = new FileListEntry("", bibtexKey != null ? file.getPath() : "",
                Globals.prefs.getExternalFileTypeByExt(suffix));
        editor = new FileListEntryEditor(frame, entry, true, false, metaData);
        editor.getProgressBar().setIndeterminate(true);
        editor.setOkEnabled(false);
        editor.setExternalConfirm(new ConfirmCloseFileListEntryEditor() {
            public boolean confirmClose(FileListEntry entry) {
                File f = directory != null ? expandFilename(directory, entry.getLink())
                        : new File(entry.getLink());
                if (f.isDirectory()) {
                    JOptionPane.showMessageDialog(frame,
                            Globals.lang("Target file cannot be a directory."), Globals.lang("Download file"),
                            JOptionPane.ERROR_MESSAGE);
                    return false;
                }
                if (f.exists()) {
                    return JOptionPane.showConfirmDialog
                        (frame, "'"+f.getName()+"' "+Globals.lang("exists. Overwrite file?"),
                        Globals.lang("Download file"), JOptionPane.OK_CANCEL_OPTION)
                            == JOptionPane.OK_OPTION;
                } else
                    return true;
            }
        });
        editor.setVisible(true);
        
        if (editor.okPressed()) {
            File toFile = directory != null ? expandFilename(directory, entry.getLink())
                    : new File(entry.getLink());
            String dirPrefix;
            if (directory != null) {
                if (!directory.endsWith(System.getProperty("file.separator")))
                    dirPrefix = directory+System.getProperty("file.separator");
                else
                    dirPrefix = directory;
            } else
                dirPrefix = null;

            try {
                boolean success = Util.copyFile(tmp, toFile, true);
                if (!success) {
                    
                    System.out.println("File already exists! DownloadExternalFile.download()");
                }

                
                
                if ((directory != null) && entry.getLink().startsWith(directory) &&
                        (entry.getLink().length() > dirPrefix.length())) {
                    entry.setLink(entry.getLink().substring(dirPrefix.length()));
                }

                callback.downloadComplete(entry);
            } catch (IOException ex) {
                ex.printStackTrace();
            }

            tmp.delete();
        }
        else {
            
            if (downloadFinished)
                tmp.delete();
        }

    }

    
    private File expandFilename(String directory, String link) {
        File toFile = new File(link);
        
        String dirPrefix = directory+System.getProperty("file.separator");
        if (!toFile.isAbsolute()) {
            toFile = new File(dirPrefix+link);
        }
        return toFile;
    }

    
    public void downloadFinished() {
        downloadFinished = true;
        editor.getProgressBar().setVisible(false);
        editor.getProgressBarLabel().setVisible(false);
        editor.setOkEnabled(true);
        editor.getProgressBar().setValue(editor.getProgressBar().getMaximum());
    }

    public String getSuggestedFileName(String res, String suffix) {
        
        String plannedName = bibtexKey;
        if (suffix.length() > 0)
            plannedName += "." + suffix;

        
        if (Globals.ON_WIN) {
            plannedName = plannedName.replaceAll(
                    "\\?|\\*|\\<|\\>|\\||\\\"|\\:|\\.$|\\[|\\]", "");
        } else if (Globals.ON_MAC) {
            plannedName = plannedName.replaceAll(":", "");
        }

        return plannedName;
    }

    
    public String getSuffix(final String link) {
        String strippedLink = link;
        try {
            
            URL url = new URL(link);
            if ((url.getQuery() != null) && (url.getQuery().length() < link.length()-1)) {
                strippedLink = link.substring(0, link.length()-url.getQuery().length()-1);
            }
        } catch (MalformedURLException e) {
            
            
        }
        
        String suffix;
        int index = strippedLink.lastIndexOf('.');
        if ((index <= 0) || (index == strippedLink.length() - 1)) 
            suffix = null;
        else suffix = strippedLink.substring(index + 1);
        if (Globals.prefs.getExternalFileTypeByExt(suffix) != null) {
            return suffix;
        }
        else {
            
            
            index = link.lastIndexOf('.');
            if ((index <= 0) || (index == strippedLink.length() - 1)) {
                
                
                
                if (suffix.indexOf('/') > 0)
                    return "";
                else
                    return suffix; 
            }
            else {
                 
                
                if (link.substring(index + 1).indexOf('/') > 0)
                    return "";
                else
                    return link.substring(index + 1);
            }
        }

    }

    public String getFileDirectory(String link) {
        return metaData.getFileDirectory(GUIGlobals.FILE_FIELD);
    }

    
    public interface DownloadCallback {
        public void downloadComplete(FileListEntry file);
    }
}
