package net.sf.jabref.external;

import java.net.URL;
import java.net.MalformedURLException;
import java.io.IOException;


public class ScienceDirectPdfDownload implements FullTextFinder {

    

    public ScienceDirectPdfDownload() {

    }

    public boolean supportsSite(URL url) {
        return url.getHost().toLowerCase().indexOf("www.sciencedirect.com") != -1;
    }



    public URL findFullTextURL(URL url) throws IOException {
        String pageSource = FindFullText.loadPage(url);
        
        int index = pageSource.indexOf("PDF (");
        
        if (index > -1) {
            String leading = pageSource.substring(0, index);
            
            index = leading.toLowerCase().lastIndexOf("<a href=");
            
            if ((index > -1) && (index+9 < leading.length())) {
                int endIndex = leading.indexOf("\"", index+9);

                try {
                    return new URL(leading.substring(index+9, endIndex));
                    
                } catch (MalformedURLException e) {
                    return null;
                }
            }
            return null;
        } else
            return null;
    }
}
