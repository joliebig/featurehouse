


package com.lowagie.text;


public class ExceptionConverter extends RuntimeException {
    private static final long serialVersionUID = 8657630363395849399L;
    
    private Exception ex;
    
    private String prefix;

    
    public ExceptionConverter(Exception ex) {
        this.ex = ex;
        prefix = (ex instanceof RuntimeException) ? "" : "ExceptionConverter: ";
    }

    
    public Exception getException() {
        return ex;
    }

    
    public String getMessage() {
        return ex.getMessage();
    }

    
    public String getLocalizedMessage() {
        return ex.getLocalizedMessage();
    }

    
    public String toString() {
        return prefix + ex;
    }

    
    public void printStackTrace() {
        printStackTrace(System.err);
    }

    
    public void printStackTrace(java.io.PrintStream s) {
        synchronized (s) {
            s.print(prefix);
            ex.printStackTrace(s);
        }
    }

    
    public void printStackTrace(java.io.PrintWriter s) {
        synchronized (s) {
            s.print(prefix);
            ex.printStackTrace(s);
        }
    }

    
    public Throwable fillInStackTrace() {
        return this;
    }
}