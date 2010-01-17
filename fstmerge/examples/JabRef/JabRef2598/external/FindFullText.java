package net.sf.jabref.external; 

import java.io.File; 
import java.io.FileWriter; 
import java.io.IOException; 
import java.io.InputStreamReader; 
import java.io.Reader; 
import java.net.HttpURLConnection; 
import java.net.MalformedURLException; 
import java.net.URL; 
import java.net.URLConnection; 
import java.util.ArrayList; 
import java.util.List; 

import net.sf.jabref.BibtexEntry; 
import net.sf.jabref.Globals; 
import net.sf.jabref.net.URLDownload; 


public  class  FindFullText {
	

    public final static int
        FOUND_PDF = 0,
        WRONG_MIME_TYPE = 1,
        UNKNOWN_DOMAIN = 2,
        LINK_NOT_FOUND = 3,
        IO_EXCEPTION = 4,
        NO_URLS_DEFINED = 5;

	

    List<FullTextFinder> finders = new ArrayList<FullTextFinder>();

	


    public FindFullText() {
        finders.add(new ScienceDirectPdfDownload());
        finders.add(new SpringerLinkPdfDownload());
    }

	

    public FindResult findFullText(BibtexEntry entry) {
        String urlText = entry.getField("url");
        String doiText = entry.getField("doi");
        
        if ((doiText != null) && (doiText.trim().length() > 0)) {
            FindResult resDoi = lookForFullTextAtURL(Globals.DOI_LOOKUP_PREFIX+doiText);
            if (resDoi.status == FOUND_PDF)
                return resDoi;
            
            else if ((urlText != null) && (urlText.trim().length() > 0)) {
                FindResult resUrl = lookForFullTextAtURL(urlText);
                if (resUrl.status == FOUND_PDF)
                    return resUrl;
                else {
                    return resDoi; 
                                   
                }
            }
            else return resDoi;
        }
        
        else if ((urlText != null) && (urlText.trim().length() > 0)) {
            return lookForFullTextAtURL(urlText);
        }
        
        else return new FindResult(NO_URLS_DEFINED, null);
    }

	

    private FindResult lookForFullTextAtURL(String urlText) {
        try {
            URL url = new URL(urlText);
            url = resolveRedirects(url, 0);
            boolean domainKnown = false;
            for (FullTextFinder finder : finders) {
                if (finder.supportsSite(url)) {
                    domainKnown = true;
                    URL result = finder.findFullTextURL(url);
                    if (result != null) {
                        
                        
                        
                        try {
                            URLDownload udl = new URLDownload(null, result, null);
                            udl.openConnectionOnly();

                            String mimeType = udl.getMimeType();
                            if ((mimeType != null) && (mimeType.toLowerCase().equals("application/pdf"))) {
                                return new FindResult(result, url);
                            }
                            else {
                                udl = new URLDownload(null, result, new File("page.html"));
                                udl.download();
                                return new FindResult(WRONG_MIME_TYPE, url);
                            }
                        } catch (IOException ex) {
                            ex.printStackTrace();
                            return new FindResult(IO_EXCEPTION, url);
                        }
                    }

                }
            }
            if (!domainKnown)
                return new FindResult(UNKNOWN_DOMAIN, url);
            else
                return new FindResult(LINK_NOT_FOUND, url);
        } catch (MalformedURLException e) {
            e.printStackTrace();

        } catch (IOException e) {
          e.printStackTrace();
        }

        return null;
    }

	

    
    private URL resolveRedirects(URL url, int redirectCount) throws IOException {
        URLConnection uc = url.openConnection();
        if (uc instanceof HttpURLConnection) {
            HttpURLConnection huc = (HttpURLConnection)uc;
            huc.setInstanceFollowRedirects(false);
            huc.connect();
            int responseCode = huc.getResponseCode();
            String location = huc.getHeaderField("location");
            huc.disconnect();
            if ((responseCode == HttpURLConnection.HTTP_MOVED_TEMP) && (redirectCount < 5)) {
                
                
                try {
                    URL newUrl = new URL(location);
                    return resolveRedirects(newUrl, redirectCount+1);
                } catch (MalformedURLException ex) {
                    return url; 
                    
                    
                }

            }
            else return url;

        }
        else return url;
    }

	

    public static String loadPage(URL url) throws IOException {
        Reader in = null;
        URLConnection uc;
        HttpURLConnection huc = null;
        try {
            uc = url.openConnection();
            if (uc instanceof HttpURLConnection) {
                huc = (HttpURLConnection)uc;
                huc.setInstanceFollowRedirects(false);
                huc.connect();

                in = new InputStreamReader(huc.getInputStream());
                StringBuilder sb = new StringBuilder();
                int c;
                while ((c = in.read()) != -1)
                    sb.append((char)c);
                return sb.toString();
            }
            else
                return null; 
        } finally {
            try {
                if (in != null) in.close();
                if (huc != null) huc.disconnect();
            } catch (IOException ex) { ex.printStackTrace(); }
        }

    }

	

    public static  class  FindResult {
		
        public URL url;

		
        public String host = null;

		
        public int status;

		

        public FindResult(URL url, URL originalUrl) {
            this.url = url;
            host = originalUrl.getHost();
            this.status = FOUND_PDF;
        }

		
        public FindResult(int status, URL originalUrl) {
            this.url = null;
            this.status = status;
            this.host = originalUrl.getHost();
        }


	}

	


    public static void dumpToFile(String text, File f) {
         try {
             FileWriter fw = new FileWriter(f);
             fw.write(text);
             fw.close();
         } catch (IOException e) {
             e.printStackTrace();

         }
    }


}
