
package com.lowagie.text.rtf.parser.exceptions;



public class RtfParserException extends Exception {
    private static final long serialVersionUID = 2857489935812968235L;
    
    private Exception ex;

    
    public RtfParserException(Exception ex) {
        this.ex = ex;
    }
    
    
    
    
    public RtfParserException() {
        super();
    }
    
    
    public RtfParserException(String message) {
        super(message);
    }

    
    public String getMessage() {
        if (ex == null)
            return super.getMessage();
        else
            return ex.getMessage();
    }

    
    public String getLocalizedMessage() {
        if (ex == null)
            return super.getLocalizedMessage();
        else
            return ex.getLocalizedMessage();
    }

    
    public String toString() {
        if (ex == null)
            return super.toString();
        else
            return split(getClass().getName()) + ": " + ex;
    }

    
    public void printStackTrace() {
        printStackTrace(System.err);
    }

    
    public void printStackTrace(java.io.PrintStream s) {
        if (ex == null)
            super.printStackTrace(s);
        else {
            synchronized (s) {
                s.print(split(getClass().getName()) + ": ");
                ex.printStackTrace(s);
            }
        }
    }

    
    public void printStackTrace(java.io.PrintWriter s) {
        if (ex == null)
            super.printStackTrace(s);
        else {
            synchronized (s) {
                s.print(split(getClass().getName()) + ": ");
                ex.printStackTrace(s);
            }
        }
    }

    
    private static String split(String s) {
        int i = s.lastIndexOf('.');
        if (i < 0)
            return s;
        else
            return s.substring(i + 1);
    }
}
