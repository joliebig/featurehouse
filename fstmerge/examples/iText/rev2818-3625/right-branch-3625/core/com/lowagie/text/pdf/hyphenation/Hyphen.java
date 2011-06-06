

package com.lowagie.text.pdf.hyphenation;

import java.io.Serializable;



public class Hyphen implements Serializable {
    private static final long serialVersionUID = -7666138517324763063L;
    public String preBreak;
    public String noBreak;
    public String postBreak;

    Hyphen(String pre, String no, String post) {
        preBreak = pre;
        noBreak = no;
        postBreak = post;
    }

    Hyphen(String pre) {
        preBreak = pre;
        noBreak = null;
        postBreak = null;
    }

    public String toString() {
        if (noBreak == null 
                && postBreak == null 
                && preBreak != null
                && preBreak.equals("-")) {
            return "-";
                }
        StringBuffer res = new StringBuffer("{");
        res.append(preBreak);
        res.append("}{");
        res.append(postBreak);
        res.append("}{");
        res.append(noBreak);
        res.append('}');
        return res.toString();
    }

}
