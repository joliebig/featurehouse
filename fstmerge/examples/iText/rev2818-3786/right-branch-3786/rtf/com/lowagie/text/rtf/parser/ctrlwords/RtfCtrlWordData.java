

package com.lowagie.text.rtf.parser.ctrlwords;



public class RtfCtrlWordData implements Cloneable {
    public String prefix = "";
    public String suffix = "";
    
    public String ctrlWord = "";
    
    public boolean hasParam = false;
    
    public String param = "";
    
    public boolean isNeg = false;
    
    public boolean newGroup = false;
    
    public boolean modified = false;
    
    public int ctrlWordType = RtfCtrlWordType.UNIDENTIFIED;
    public String specialHandler = "";
    
    
    public int intValue() {
        int value;
        value = Integer.parseInt(this.param);
        if(this.isNeg) value = (-value);
        return value;
    }
    
    public Integer toInteger() {
        Integer value;
        value = new Integer(this.isNeg ? Integer.parseInt(this.param)*-1 : Integer.parseInt(this.param));
        return value;
    }
    

    
    public long longValue() {
        long value;
        value = Long.parseLong(this.param);
        if(this.isNeg) value = (-value);
        return value;
    }
    
    public Long toLong() {
        Long value;
        value = new Long(this.isNeg ? Long.parseLong(this.param)*-1 : Long.parseLong(this.param));
        return value;
    }
    
    public String toString() {
        String out = "";
        out = this.prefix + this.ctrlWord;
        if(this.hasParam) {
            if(this.isNeg) out += "-";
            out += this.param; 
        }
        out += this.suffix;
        return out;
    }
    
    public Object clone() throws CloneNotSupportedException{
        Object cl = super.clone();
        return cl;
    }
}
