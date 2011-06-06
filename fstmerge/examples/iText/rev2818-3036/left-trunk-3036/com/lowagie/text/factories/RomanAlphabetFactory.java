

package com.lowagie.text.factories;


public class RomanAlphabetFactory {

    
    public static final String getString(int index) {
        if (index < 1) throw new NumberFormatException(
                "You can't translate a negative number into an alphabetical value.");
        
        index--;
        int bytes = 1;
        int start = 0;
        int symbols = 26;  
        while(index >= symbols + start) {
            bytes++;
            start += symbols;
            symbols *= 26;
        }
              
        int c = index - start;
        char[] value = new char[bytes];
        while(bytes > 0) {
            value[--bytes] = (char)( 'a' + (c % 26));
            c /= 26;
        }
        
        return new String(value);
    }
    
    
    public static final String getLowerCaseString(int index) {
        return getString(index);        
    }
    
    
    public static final String getUpperCaseString(int index) {
        return getString(index).toUpperCase();        
    }

    
    
    public static final String getString(int index, boolean lowercase) {
        if (lowercase) {
            return getLowerCaseString(index);
        }
        else {
            return getUpperCaseString(index);
        }
    }
    
    
    public static void main(String[] args) {
        for (int i = 1; i < 32000; i++) {
            System.out.println(getString(i));
        }
    }
}