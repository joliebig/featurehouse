package net.sf.jabref.export;

import javax.swing.filechooser.FileFilter;
import java.io.File;


public class ExportFileFilter extends FileFilter implements Comparable {
    private ExportFormat format;
    private String extension, name;

    public ExportFileFilter(ExportFormat format) {
        this.format = format;
        this.extension = format.getExtension();
        this.name = format.getDisplayName()+" (*"+format.getExtension()+")";
    }

    public ExportFormat getExportFormat() {
        return format;
    }

    public boolean accept(File file) {
        if (file.isDirectory())
            return true;
        else
            return file.getPath().toLowerCase().endsWith(extension);
    }

    public String getDescription() {
        return name;
    }

    public int compareTo(Object o) {
        return name.compareTo(((ExportFileFilter)o).name);
    }
}
