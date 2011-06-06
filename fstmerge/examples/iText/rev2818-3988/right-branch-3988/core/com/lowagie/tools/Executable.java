
package com.lowagie.tools;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;


public class Executable {
    
    
    public static String acroread = null;

    
    
    private static Process action(final String fileName,
            String parameters, boolean waitForTermination) throws IOException {
        Process process = null;
        if (parameters.trim().length() > 0) {
            parameters = " " + parameters.trim();
        }
        else {
            parameters = "";
        }
        if (acroread != null) {
            process = Runtime.getRuntime().exec(
                    acroread + parameters + " \"" + fileName + "\"");
        }
        else if (isWindows()) {
            if (isWindows9X()) {
                process = Runtime.getRuntime().exec(
                        "command.com /C start acrord32" + parameters + " \"" + fileName + "\"");
            }
            else {
                process = Runtime.getRuntime().exec(
                    "cmd /c start acrord32" + parameters + " \"" + fileName + "\"");
            }
        }
        else if (isMac()) {
            if (parameters.trim().length() == 0) {
                process = Runtime.getRuntime().exec(
                    new String[] { "/usr/bin/open", fileName });
            }
            else {
                process = Runtime.getRuntime().exec(
                        new String[] { "/usr/bin/open", parameters.trim(), fileName });
            }
        }
        try {
            if (process != null && waitForTermination)
                process.waitFor();
        } catch (InterruptedException ie) {
        }
        return process;
    }
    
    
    public static final Process openDocument(String fileName,
            boolean waitForTermination) throws IOException {
        return action(fileName, "", waitForTermination);
    }

    
    public static final Process openDocument(File file,
            boolean waitForTermination) throws IOException {
        return openDocument(file.getAbsolutePath(), waitForTermination);
    }

    
    public static final Process openDocument(String fileName) throws IOException {
        return openDocument(fileName, false);
    }

    
    public static final Process openDocument(File file) throws IOException {
        return openDocument(file, false);
    }
    
    
    public static final Process printDocument(String fileName,
            boolean waitForTermination) throws IOException {
        return action(fileName, "/p", waitForTermination);
    }

    
    public static final Process printDocument(File file,
            boolean waitForTermination) throws IOException {
        return printDocument(file.getAbsolutePath(), waitForTermination);
    }

    
    public static final Process printDocument(String fileName) throws IOException {
        return printDocument(fileName, false);
    }

    
    public static final Process printDocument(File file) throws IOException {
        return printDocument(file, false);
    }
    
    
    public static final Process printDocumentSilent(String fileName,
            boolean waitForTermination) throws IOException {
        return action(fileName, "/p /h", waitForTermination);
    }

    
    public static final Process printDocumentSilent(File file,
            boolean waitForTermination) throws IOException {
        return printDocumentSilent(file.getAbsolutePath(), waitForTermination);
    }

    
    public static final Process printDocumentSilent(String fileName) throws IOException {
        return printDocumentSilent(fileName, false);
    }

    
    public static final Process printDocumentSilent(File file) throws IOException {
        return printDocumentSilent(file, false);
    }
    
    
    public static final void launchBrowser(String url) throws IOException {
        try {
            if (isMac()) {
                Class<?> macUtils = Class.forName("com.apple.mrj.MRJFileUtils");
                Method openURL = macUtils.getDeclaredMethod("openURL", new Class[] {String.class});
                openURL.invoke(null, new Object[] {url});
            }
            else if (isWindows())
                Runtime.getRuntime().exec("rundll32 url.dll,FileProtocolHandler " + url);
            else { 
                String[] browsers = {
                   "firefox", "opera", "konqueror", "mozilla", "netscape" };
                String browser = null;
                for (int count = 0; count < browsers.length && browser == null; count++)
                   if (Runtime.getRuntime().exec(new String[] {"which", browsers[count]}).waitFor() == 0)
                      browser = browsers[count];
                if (browser == null)
                   throw new Exception("Could not find web browser.");
                else
                   Runtime.getRuntime().exec(new String[] {browser, url});
                }
             }
          catch (Exception e) {
             throw new IOException("Error attempting to launch web browser");
          }
    }

    
    public static boolean isWindows() {
        String os = System.getProperty("os.name").toLowerCase();
        return os.indexOf("windows") != -1 || os.indexOf("nt") != -1;
    }

    
    public static boolean isWindows9X() {
        String os = System.getProperty("os.name").toLowerCase();
        return os.equals("windows 95") || os.equals("windows 98");
    }

    
    public static boolean isMac() {
        String os = System.getProperty("os.name").toLowerCase();
        return os.indexOf("mac") != -1;
    }

    
    public static boolean isLinux() {
        String os = System.getProperty("os.name").toLowerCase();
        return os.indexOf("linux") != -1;
    }
}