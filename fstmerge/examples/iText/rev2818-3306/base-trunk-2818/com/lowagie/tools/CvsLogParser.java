
package com.lowagie.tools;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StreamTokenizer;
import java.util.Enumeration;


public class CvsLogParser implements Enumeration {

    
    protected StreamTokenizer st;
    
    
    protected boolean changes = false;
    
    
    protected boolean more = false;
    
    
    public CvsLogParser(String file) throws FileNotFoundException {
        BufferedReader r = new BufferedReader(new InputStreamReader(new FileInputStream(file)));
        st = new StreamTokenizer(r);
        st.eolIsSignificant(true);
        st.ordinaryChar('/');
        st.ordinaryChar('\'');
        more = true;
    }

    
    public boolean hasMoreElements() {
        return more;
    }
    
    
    public Object nextElement(){
        StringBuffer token = new StringBuffer();
        StringBuffer line = new StringBuffer();
        boolean moreToken = true;
        changes = false;
        try {
            while (more && moreToken) {
                st.nextToken();
                switch(st.ttype) {
                case StreamTokenizer.TT_EOF:
                    more = false;
                case StreamTokenizer.TT_EOL:
                    token.append(line.toString());
                    if (line.toString().endsWith("=============================================================================")) {
                        moreToken = false;
                    }
                    else {
                        line = new StringBuffer("\n");
                    }
                    break;
                case StreamTokenizer.TT_WORD:
                    line.append(st.sval);
                    line.append(' ');
                    break;
                case StreamTokenizer.TT_NUMBER:
                    if (st.nval > 0 && line.toString().endsWith("selected revisions :")) {
                        changes = true;
                    }
                    line.append(st.nval);
                    break;
                default:
                    line.append((char) st.ttype);
                }
            }
            return token.toString();
        }
        catch(IOException ioe) {
            more = false;
            return "";
        }
    }

    
    private boolean hasChanged() {
        return changes;
    }
    
    
    
    public static void main(String[] args) {
        try {
            CvsLogParser p = new CvsLogParser(args[0]);
            String token;
            while (p.hasMoreElements()) {
                token = (String) p.nextElement();
                if (p.hasChanged()) {
                    System.out.println(token);
                }
            }
        }
        catch(Exception e) {
            e.printStackTrace();
        }
    }
}
