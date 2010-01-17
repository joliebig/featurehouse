package net.sf.jabref.export.layout.format; 

import net.sf.jabref.export.layout.LayoutFormatter; 


public  class  LastPage implements  LayoutFormatter {
	

    <<<<<<< C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var1_44365
public String format(String s) {
        if (s == null)
			return "";
		String[] pageParts = s.split("[\\-]+");
		if (pageParts.length == 2)
            return pageParts[1];
        
        
        else return pageParts[0];
        

    }
=======
public String format(String s) {
        if (s == null)
			return "";
		String[] pageParts = s.split("[\\-]+");
		if (pageParts.length == 2)
            return pageParts[1];
        else return "";

    }
>>>>>>> C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var2_44367



}
