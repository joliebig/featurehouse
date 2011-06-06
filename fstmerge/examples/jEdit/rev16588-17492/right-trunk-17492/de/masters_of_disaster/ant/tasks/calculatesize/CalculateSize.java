package de.masters_of_disaster.ant.tasks.calculatesize;

import java.io.File;
import java.util.Enumeration;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.MatchingTask;
import org.apache.tools.ant.types.FileSet;


public class CalculateSize extends MatchingTask {
    String realSizeProperty = null;
    String diskSizeProperty = null;
    Vector fileSets = new Vector();
    File baseDir;

    
    public FileSet createFileSet() {
        FileSet fileSet = new FileSet();
        fileSets.addElement(fileSet);
        return fileSet;
    }

    
    public void setBaseDir(File baseDir) {
        this.baseDir = baseDir;
        fileset.setDir(baseDir);
    }

    
    public void setRealSizeProperty(String realSizeProperty) {
        this.realSizeProperty = realSizeProperty;
    }

    
    public void setDiskSizeProperty(String diskSizeProperty) {
        this.diskSizeProperty = diskSizeProperty;
    }

    
    public void execute() throws BuildException {
        if ((null == realSizeProperty) && (null == diskSizeProperty)) {
            throw new BuildException("realSizeProperty or diskSizeProperty must be set for <CalculateSize>");
        }

        if (null != baseDir) {
            
            fileSets.addElement(fileset);
        }

        long realSize = 0;
        long diskSize = 0;
        for (Enumeration e=fileSets.elements() ; e.hasMoreElements() ; ) {
            FileSet fileSet = (FileSet)e.nextElement();
            String[] files = fileSet.getDirectoryScanner(getProject()).getIncludedFiles();
            File fileSetDir = fileSet.getDir(getProject());
            for (int i=0, c=files.length ; i<c ; i++) {
                long fileLength = new File(fileSetDir,files[i]).length();
                realSize += fileLength / 1024;
                diskSize += (fileLength / 4096 + 1) * 4;
            }
        }
        if (null != realSizeProperty) {
            getProject().setNewProperty(realSizeProperty,Long.toString(realSize));
        }
        if (null != diskSizeProperty) {
            getProject().setNewProperty(diskSizeProperty,Long.toString(diskSize));
        }
    }
}
