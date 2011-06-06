
package com.lowagie.text.factories;

import com.lowagie.text.SpecialSymbol;


public class GreekNumberFactory {
    
    public static final String getString(int index) {
        return getString(index, true);
    }
    
    
    public static final String getLowerCaseString(int index) {
        return getString(index);        
    }
    
    
    public static final String getUpperCaseString(int index) {
        return getString(index).toUpperCase();        
    }

    
    public static final String getString(int index, boolean lowercase) {
        if (index < 1) return "";
        index--;
            
        int bytes = 1;
        int start = 0;
        int symbols = 24;  
           while(index >= symbols + start) {
               bytes++;
               start += symbols;
               symbols *= 24;
           }
                 
           int c = index - start;
           char[] value = new char[bytes];
           while(bytes > 0) {
               bytes--;
               value[bytes] = (char)(c % 24);
               if (value[bytes] > 16) value[bytes]++;
               value[bytes] += (lowercase ? 945 : 913);
               value[bytes] = SpecialSymbol.getCorrespondingSymbol(value[bytes]);
               c /= 24;
           }
           
           return String.valueOf(value);
    }
    
    
    public static void main(String[] args) {
        for (int i = 1; i < 1000; i++) {
            System.out.println(getString(i));
        }
    }
}
