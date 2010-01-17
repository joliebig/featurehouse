package net.sf.jabref.export;

import net.sf.jabref.Globals;
import net.sf.jabref.Util;
import net.sf.jabref.GUIGlobals;

import java.io.File;
import java.io.IOException;
import java.io.FileOutputStream;
import java.nio.charset.UnsupportedCharsetException;


public class SaveSession {

    private static final String TEMP_PREFIX = "jabref";
    private static final String TEMP_SUFFIX = "save.bib";
    File file, tmp, backupFile;
    String encoding;
    boolean backup;
    VerifyingWriter writer;

    public SaveSession(File file, String encoding, boolean backup) throws IOException,
        UnsupportedCharsetException {
        this.file = file;
        tmp = File.createTempFile(TEMP_PREFIX, TEMP_SUFFIX);
        this.backup = backup;
        this.encoding = encoding;
        writer = new VerifyingWriter(new FileOutputStream(tmp), encoding);
    }

    public VerifyingWriter getWriter() {
        return writer;
    }

    public String getEncoding() {
        return encoding;
    }

    public void commit() throws SaveException {
        if (file == null)
            return;
        if (file.exists() && backup) {
            String name = file.getName();
            String path = file.getParent();
            File backupFile = new File(path, name + GUIGlobals.backupExt);
            try {
                Util.copyFile(file, backupFile, true);
            } catch (IOException ex) {
                throw new SaveException(Globals.lang("Save failed during backup creation")+": "+ex.getMessage());
            }
        }
        try {
            Util.copyFile(tmp, file, true);
        } catch (IOException ex2) {
            
            
            
            
            throw new SaveException(Globals.lang("Save failed while committing changes")+": "+ex2.getMessage());
        }

        tmp.delete();
    }

    public void cancel() throws IOException {
        tmp.delete();
    }

    public File getTemporaryFile() {
        return tmp;
    }
}
