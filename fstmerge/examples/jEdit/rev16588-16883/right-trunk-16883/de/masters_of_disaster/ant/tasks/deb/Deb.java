package de.masters_of_disaster.ant.tasks.deb;

import de.masters_of_disaster.ant.tasks.ar.Ar;
import de.masters_of_disaster.ant.tasks.ar.Ar.ArFileSet;
import java.io.File;
import java.util.Enumeration;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.taskdefs.Checksum;
import org.apache.tools.ant.taskdefs.Echo;
import org.apache.tools.ant.taskdefs.Echo.EchoLevel;
import org.apache.tools.ant.taskdefs.Mkdir;
import org.apache.tools.ant.taskdefs.MatchingTask;
import org.apache.tools.ant.taskdefs.Tar;
import org.apache.tools.ant.taskdefs.Tar.TarCompressionMethod;
import org.apache.tools.ant.taskdefs.Tar.TarFileSet;
import org.apache.tools.ant.util.FileUtils;
import org.apache.tools.ant.util.MergingMapper;
import org.apache.tools.ant.util.SourceFileScanner;


public class Deb extends MatchingTask {
    Vector controlFileSets = new Vector();
    Vector dataFileSets = new Vector();
    File baseDir;
    File destFile;
    File tempDir;
    boolean deleteTempFiles = true;
    boolean includeMd5sums = false;
    Tar controlTarGz = new Tar();
    Tar dataTarGz = new Tar();
    Ar debPackage = new Ar();

    {
        fileset = dataTarGz.createTarFileSet();
    }

    
    public TarFileSet createControlFileSet() {
        TarFileSet fileSet = controlTarGz.createTarFileSet();
        controlFileSets.addElement(fileSet);
        return fileSet;
    }

    
    public TarFileSet createDataFileSet() {
        TarFileSet fileSet = dataTarGz.createTarFileSet();
        dataFileSets.addElement(fileSet);
        return fileSet;
    }

    
    public void setDestFile(File destFile) {
        this.destFile = destFile;
        debPackage.setDestFile(destFile);
    }

    
    public void setBaseDir(File baseDir) {
        this.baseDir = baseDir;
        fileset.setDir(baseDir);
    }

    
    public void setTempDir(File tempDir) {
        this.tempDir = tempDir;
    }

    
    public void setDeleteTempFiles(boolean deleteTempFiles) {
        this.deleteTempFiles = deleteTempFiles;
    }

    
    public void setIncludeMd5sums(boolean includeMd5sums) {
        this.includeMd5sums = includeMd5sums;
    }

    
    public void execute() throws BuildException {
        prepareTask(controlTarGz);
        prepareTask(dataTarGz);
        prepareTask(debPackage);
        TarFileSet tarFileSet = controlTarGz.createTarFileSet();
        tarFileSet.setFile(new File(System.getProperty("user.dir")));
        tarFileSet.setUserName("root");
        tarFileSet.setGroup("root");
        tarFileSet.setFullpath("./");
        tarFileSet = dataTarGz.createTarFileSet();
        tarFileSet.setFile(new File(System.getProperty("user.dir")));
        tarFileSet.setUserName("root");
        tarFileSet.setGroup("root");
        tarFileSet.setFullpath("./");

        if (null == tempDir) {
            tempDir = getProject().getBaseDir();
        }

        if (null != baseDir) {
            
            dataFileSets.addElement(fileset);
        } else {
            fileset.setDir(new File(System.getProperty("user.dir")));
            fileset.setExcludes("**");
        }

        boolean controlFound = false;
        for (Enumeration e=controlFileSets.elements() ; e.hasMoreElements() ; ) {
            TarFileSet fileSet = (TarFileSet)e.nextElement();
            String[] files = fileSet.getFiles(getProject());
            int i = 0;
            int c;

            for (c=files.length ; i<c && !controlFound ; i++) {
                if (files[i].endsWith("control")
                      && (new File(fileSet.getDir(getProject()),files[i])).isFile()) {
                    controlFound = true;
                }
            }
        }
        if (!controlFound) {
            throw new BuildException("The control fileset must contain a file \"control\"", getLocation());
        }

        
        boolean upToDate = true;
        for (Enumeration e=controlFileSets.elements() ; e.hasMoreElements() ; ) {
            TarFileSet fileSet = (TarFileSet)e.nextElement();
            String[] files = fileSet.getFiles(getProject());

            if (!packageIsUpToDate(files,fileSet.getDir(getProject()))) {
                upToDate = false;
            }
        }

        for (Enumeration e=dataFileSets.elements() ; e.hasMoreElements() ; ) {
            TarFileSet fileSet = (TarFileSet)e.nextElement();
            String[] files = fileSet.getFiles(getProject());

            if (!packageIsUpToDate(files,fileSet.getDir(getProject()))) {
                upToDate = false;
            }
        }

        if (upToDate) {
            log("Nothing to do: " + destFile.getAbsolutePath()
                + " is up to date.", Project.MSG_INFO);
            return;
        }

        log("Building deb: " + destFile.getAbsolutePath(), Project.MSG_INFO);

        Mkdir mkdir = new Mkdir();
        prepareTask(mkdir);
        mkdir.setDir(tempDir);
        mkdir.perform();

        Echo echo = new Echo();
        prepareTask(echo);
        EchoLevel echoLevel = new EchoLevel();
        echoLevel.setValue("error");
        File debianBinaryFile = new File(tempDir,"debian-binary");
        echo.setFile(debianBinaryFile);
        echo.setLevel(echoLevel);
        echo.setMessage("2.0\n");
        echo.perform();

        for (Enumeration e=controlFileSets.elements() ; e.hasMoreElements() ; ) {
            TarFileSet fileSet = (TarFileSet)e.nextElement();
            String prefix = fileSet.getPrefix();
            String fullpath = fileSet.getFullpath();
            if ("".equals(fullpath) && !prefix.startsWith("./")) {
                if (prefix.startsWith("/")) {
                    fileSet.setPrefix("." + prefix);
                } else {
                    fileSet.setPrefix("./" + prefix);
                }
            }
            if ((fullpath.length() > 0) && !fullpath.startsWith("./")) {
                fileSet.setPrefix("");
                if (fullpath.startsWith("/")) {
                    fileSet.setFullpath("." + fullpath);
                } else {
                    fileSet.setFullpath("./" + fullpath);
                }
            }
            if ((0 == fileSet.getUid()) && ("" == fileSet.getUserName())) {
                fileSet.setUserName("root");
            }
            if ((0 == fileSet.getGid()) && ("" == fileSet.getGroup())) {
                fileSet.setGroup("root");
            }
        }

        for (Enumeration e=dataFileSets.elements() ; e.hasMoreElements() ; ) {
            TarFileSet fileSet = (TarFileSet)e.nextElement();
            String prefix = fileSet.getPrefix();
            String fullpath = fileSet.getFullpath();
            if ("".equals(fullpath) && !prefix.startsWith("./")) {
                if (prefix.startsWith("/")) {
                    fileSet.setPrefix("." + prefix);
                } else {
                    fileSet.setPrefix("./" + prefix);
                }
            }
            if ((fullpath.length() > 0) && !fullpath.startsWith("./")) {
                fileSet.setPrefix("");
                if (fullpath.startsWith("/")) {
                    fileSet.setFullpath("." + fullpath);
                } else {
                    fileSet.setFullpath("./" + fullpath);
                }
            }
            if ((0 == fileSet.getUid()) && ("" == fileSet.getUserName())) {
                fileSet.setUserName("root");
            }
            if ((0 == fileSet.getGid()) && ("" == fileSet.getGroup())) {
                fileSet.setGroup("root");
            }
        }

        File md5sumsFile = new File(tempDir,"md5sums");
        if (includeMd5sums) {
            Checksum md5 = new Checksum();
            prepareTask(md5);
            int md5Count = 0;
            StringBuffer md5sums = new StringBuffer();
            for (Enumeration e=dataFileSets.elements() ; e.hasMoreElements() ; ) {
                TarFileSet fileSet = (TarFileSet)e.nextElement();
                String[] files = fileSet.getDirectoryScanner(getProject()).getIncludedFiles();
                File fileSetDir = fileSet.getDir(getProject());
                for (int i=0, c=files.length ; i<c ; i++) {
                    md5.setFile(new File(fileSetDir,files[i]));
                    md5.setProperty("md5_"+md5Count);
                    md5.perform();
                    md5sums.append(getProject().getProperty("md5_"+md5Count)).append("  ");
                    String fullpath = fileSet.getFullpath();
                    if (fullpath.length() > 0) {
                        md5sums.append(fullpath.substring(2));
                    } else {
                        md5sums.append(fileSet.getPrefix().substring(2)).append(files[i].replace('\\','/'));
                    }
                    md5sums.append("\n");
                    md5Count++;
                }
            }
            echo.setFile(md5sumsFile);
            echo.setMessage(md5sums.toString());
            echo.perform();
            tarFileSet = controlTarGz.createTarFileSet();
            tarFileSet.setFile(md5sumsFile);
            tarFileSet.setUserName("root");
            tarFileSet.setGroup("root");
            tarFileSet.setPrefix("./");
        }

        TarCompressionMethod tarCompressionMethod = new TarCompressionMethod();
        tarCompressionMethod.setValue("gzip");
        controlTarGz.setCompression(tarCompressionMethod);
        File controlTarGzFile = new File(tempDir,"control.tar.gz");
        controlTarGz.setDestFile(controlTarGzFile);
        controlTarGz.perform();

        dataTarGz.setCompression(tarCompressionMethod);
        File dataTarGzFile = new File(tempDir,"data.tar.gz");
        dataTarGz.setDestFile(dataTarGzFile);
        dataTarGz.perform();

        FileUtils.delete(destFile);
        ArFileSet fileSet = debPackage.createArFileSet();
        fileSet.setFile(debianBinaryFile);
        fileSet = debPackage.createArFileSet();
        fileSet.setFile(controlTarGzFile);
        fileSet = debPackage.createArFileSet();
        fileSet.setFile(dataTarGzFile);
        debPackage.perform();

        if (deleteTempFiles) {
            FileUtils.delete(debianBinaryFile);
            FileUtils.delete(controlTarGzFile);
            FileUtils.delete(dataTarGzFile);
            FileUtils.delete(md5sumsFile);
        }
    }

    
    protected boolean packageIsUpToDate(String[] files, File dir) {
        SourceFileScanner sfs = new SourceFileScanner(this);
        MergingMapper mm = new MergingMapper();
        mm.setTo(destFile.getAbsolutePath());
        return sfs.restrict(files, dir, null, mm).length == 0;
    }

    
    protected void prepareTask(Task task) {
        task.setProject(getProject());
        task.setOwningTarget(getOwningTarget());
        task.setTaskName(getTaskName());
        task.setTaskType(getTaskType());
    }
}
