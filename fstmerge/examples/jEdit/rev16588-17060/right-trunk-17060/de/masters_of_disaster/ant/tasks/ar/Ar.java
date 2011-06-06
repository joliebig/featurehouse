package de.masters_of_disaster.ant.tasks.ar;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.MatchingTask;
import org.apache.tools.ant.types.EnumeratedAttribute;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.util.FileUtils;
import org.apache.tools.ant.util.MergingMapper;
import org.apache.tools.ant.util.SourceFileScanner;
import org.apache.tools.zip.UnixStat;


public class Ar extends MatchingTask {
    File destFile;
    File baseDir;

    private ArLongFileMode longFileMode = new ArLongFileMode();

    Vector filesets = new Vector();

    
    private boolean longWarningGiven = false;

    
    public ArFileSet createArFileSet() {
        ArFileSet fileset = new ArFileSet();
        filesets.addElement(fileset);
        return fileset;
    }


    
    public void setDestFile(File destFile) {
        this.destFile = destFile;
    }

    
    public void setBasedir(File baseDir) {
        this.baseDir = baseDir;
    }

    
    public void setLongfile(ArLongFileMode mode) {
        this.longFileMode = mode;
    }

    
    public void execute() throws BuildException {
        if (destFile == null) {
            throw new BuildException("destFile attribute must be set!",
                                     getLocation());
        }

        if (destFile.exists() && destFile.isDirectory()) {
            throw new BuildException("destFile is a directory!",
                                     getLocation());
        }

        if (destFile.exists() && !destFile.canWrite()) {
            throw new BuildException("Can not write to the specified destFile!",
                                     getLocation());
        }

        Vector savedFileSets = (Vector) filesets.clone();
        try {
            if (baseDir != null) {
                if (!baseDir.exists()) {
                    throw new BuildException("basedir does not exist!",
                                             getLocation());
                }

                
                ArFileSet mainFileSet = new ArFileSet(fileset);
                mainFileSet.setDir(baseDir);
                filesets.addElement(mainFileSet);
            }

            if (filesets.size() == 0) {
                throw new BuildException("You must supply either a basedir "
                                         + "attribute or some nested filesets.",
                                         getLocation());
            }

            
            
            boolean upToDate = true;
            for (Enumeration e = filesets.elements(); e.hasMoreElements();) {
                ArFileSet fs = (ArFileSet) e.nextElement();
                String[] files = fs.getFiles(getProject());

                if (!archiveIsUpToDate(files, fs.getDir(getProject()))) {
                    upToDate = false;
                }

                for (int i = 0; i < files.length; ++i) {
                    if (destFile.equals(new File(fs.getDir(getProject()),
                                                files[i]))) {
                        throw new BuildException("An ar file cannot include "
                                                 + "itself", getLocation());
                    }
                }
            }

            if (upToDate) {
                log("Nothing to do: " + destFile.getAbsolutePath()
                    + " is up to date.", Project.MSG_INFO);
                return;
            }

            log("Building ar: " + destFile.getAbsolutePath(), Project.MSG_INFO);

            ArOutputStream aOut = null;
            try {
                aOut = new ArOutputStream(
                    new BufferedOutputStream(
                        new FileOutputStream(destFile)));
                if (longFileMode.isTruncateMode()
                     || longFileMode.isWarnMode()) {
                    aOut.setLongFileMode(ArOutputStream.LONGFILE_TRUNCATE);
                } else if (longFileMode.isFailMode()
                            || longFileMode.isOmitMode()) {
                    aOut.setLongFileMode(ArOutputStream.LONGFILE_ERROR);
                } else if (longFileMode.isBsdMode()) {
                    aOut.setLongFileMode(ArOutputStream.LONGFILE_BSD);
                } else {
                    
                    aOut.setLongFileMode(ArOutputStream.LONGFILE_GNU);
                }

                longWarningGiven = false;
                for (Enumeration e = filesets.elements();
                     e.hasMoreElements();) {
                    ArFileSet fs = (ArFileSet) e.nextElement();
                    String[] files = fs.getFiles(getProject());
                    if (files.length > 1 && fs.getFullpath().length() > 0) {
                        throw new BuildException("fullpath attribute may only "
                                                 + "be specified for "
                                                 + "filesets that specify a "
                                                 + "single file.");
                    }
                    for (int i = 0; i < files.length; i++) {
                        File f = new File(fs.getDir(getProject()), files[i]);
                        arFile(f, aOut, fs);
                    }
                }
            } catch (IOException ioe) {
                String msg = "Problem creating AR: " + ioe.getMessage();
                throw new BuildException(msg, ioe, getLocation());
            } finally {
                FileUtils.close(aOut);
            }
        } finally {
            filesets = savedFileSets;
        }
    }

    
    protected void arFile(File file, ArOutputStream aOut, ArFileSet arFileSet)
        throws IOException {
        FileInputStream fIn = null;

        if (file.isDirectory()) {
            return;
        }

        String fileName = file.getName();

        String fullpath = arFileSet.getFullpath();
        if (fullpath.length() > 0) {
            fileName = fullpath.substring(fullpath.lastIndexOf('/'));
        }

        
        if (fileName.length() <= 0) {
            return;
        }

        try {
            if ((fileName.length() >= ArConstants.NAMELEN)
                  || (-1 != fileName.indexOf(' '))) {
                if (longFileMode.isOmitMode()) {
                    log("Omitting: " + fileName, Project.MSG_INFO);
                    return;
                } else if (longFileMode.isWarnMode()) {
                    if (!longWarningGiven) {
                        log("Resulting ar file contains truncated or space converted filenames",
                            Project.MSG_WARN);
                        longWarningGiven = true;
                    }
                    log("Entry: \"" + fileName + "\" longer than "
                        + ArConstants.NAMELEN + " characters or containing spaces.",
                        Project.MSG_WARN);
                } else if (longFileMode.isFailMode()) {
                    throw new BuildException("Entry: \"" + fileName
                        + "\" longer than " + ArConstants.NAMELEN
                        + "characters or containting spaces.", getLocation());
                }
            }

            ArEntry ae = new ArEntry(fileName);
            ae.setFileDate(file.lastModified());
            ae.setUserId(arFileSet.getUid());
            ae.setGroupId(arFileSet.getGid());
            ae.setMode(arFileSet.getMode());
            ae.setSize(file.length());

            aOut.putNextEntry(ae);

            fIn = new FileInputStream(file);

            byte[] buffer = new byte[8 * 1024];
            int count = 0;
            do {
                aOut.write(buffer, 0, count);
                count = fIn.read(buffer, 0, buffer.length);
            } while (count != -1);

            aOut.closeEntry();
        } finally {
            if (fIn != null) {
                fIn.close();
            }
        }
    }

    
    protected boolean archiveIsUpToDate(String[] files, File dir) {
        SourceFileScanner sfs = new SourceFileScanner(this);
        MergingMapper mm = new MergingMapper();
        mm.setTo(destFile.getAbsolutePath());
        return sfs.restrict(files, dir, null, mm).length == 0;
    }

    
    public static class ArFileSet extends FileSet {
        private String[] files = null;

        private int fileMode = UnixStat.FILE_FLAG | UnixStat.DEFAULT_FILE_PERM;
        private int    uid;
        private int    gid;
        private String fullpath = "";

        
        public ArFileSet(FileSet fileset) {
            super(fileset);
        }

        
        public ArFileSet() {
            super();
        }

        
        public String[] getFiles(Project p) {
            if (files == null) {
                DirectoryScanner ds = getDirectoryScanner(p);
                files = ds.getIncludedFiles();
            }

            return files;
        }

        
        public void setMode(String octalString) {
            this.fileMode =
                UnixStat.FILE_FLAG | Integer.parseInt(octalString, 8);
        }

        
        public int getMode() {
            return fileMode;
        }

        
        public void setUid(int uid) {
            this.uid = uid;
        }

        
        public int getUid() {
            return uid;
        }

        
        public void setGid(int gid) {
            this.gid = gid;
        }

        
        public int getGid() {
            return gid;
        }

        
        public void setFullpath(String fullpath) {
            this.fullpath = fullpath;
        }

        
        public String getFullpath() {
            return fullpath;
        }
    }

    
    public static class ArLongFileMode extends EnumeratedAttribute {
        
        public static final String
            WARN = "warn",
            FAIL = "fail",
            TRUNCATE = "truncate",
            GNU = "gnu",
            BSD = "bsd",
            OMIT = "omit";

        private final String[] validModes = {WARN, FAIL, TRUNCATE, GNU, BSD, OMIT};

        
        public ArLongFileMode() {
            super();
            setValue(WARN);
        }

        
        public String[] getValues() {
            return validModes;
        }

        
        public boolean isTruncateMode() {
            return TRUNCATE.equalsIgnoreCase(getValue());
        }

        
        public boolean isWarnMode() {
            return WARN.equalsIgnoreCase(getValue());
        }

        
        public boolean isGnuMode() {
            return GNU.equalsIgnoreCase(getValue());
        }

        
        public boolean isBsdMode() {
            return BSD.equalsIgnoreCase(getValue());
        }

        
        public boolean isFailMode() {
            return FAIL.equalsIgnoreCase(getValue());
        }

        
        public boolean isOmitMode() {
            return OMIT.equalsIgnoreCase(getValue());
        }
    }
}
