
 
package com.lowagie.text.rtf.document;


public final class RtfProtection {
    
    static final public int LEVEL_NONE = 0x0000;
    
    static final public int LEVEL_REVPROT = 0x0001; 
    
    static final public int LEVEL_ANNOTPROT = 0x0002; 
    
    static final public int LEVEL_FORMPROT = 0x0004; 
    
    static final public int LEVEL_READPROT = 0x0008; 


    
    static final public int STYLELOCK = 0x0001;
    
    static final public int STYLELOCKENFORCED = 0x0002;
    
    static final public int STYLELOCKBACKCOMP = 0x0004;
    
    static final public int AUTOFMTOVERRIDE = 0x0008;
    
    
    
    static final private int initialCodeArray[] = { 
            0xE1F0, 
            0x1D0F, 
            0xCC9C, 
            0x84C0,
            0x110C,
            0x0E10,
            0xF1CE,
            0x313E,
            0x1872,
            0xE139,
            0xD40F,
            0x84F9,
            0x280C,
            0xA96A,
            0x4EC3

    };
    
    
    static final private int encryptionMatrix [][] = {
        
         {0x1021, 0x2042, 0x4084, 0x8108, 0x1231, 0x2462, 0x48C4},
         {0x3331, 0x6662, 0xCCC4, 0x89A9, 0x0373, 0x06E6, 0x0DCC},
         {0x3730, 0x6E60, 0xDCC0, 0xA9A1, 0x4363, 0x86C6, 0x1DAD},
         {0x76B4, 0xED68, 0xCAF1, 0x85C3, 0x1BA7, 0x374E, 0x6E9C},
         {0xAA51, 0x4483, 0x8906, 0x022D, 0x045A, 0x08B4, 0x1168},
         {0x45A0, 0x8B40, 0x06A1, 0x0D42, 0x1A84, 0x3508, 0x6A10},
         {0xB861, 0x60E3, 0xC1C6, 0x93AD, 0x377B, 0x6EF6, 0xDDEC},
         {0x47D3, 0x8FA6, 0x0F6D, 0x1EDA, 0x3DB4, 0x7B68, 0xF6D0},
         {0xEB23, 0xC667, 0x9CEF, 0x29FF, 0x53FE, 0xA7FC, 0x5FD9},
         {0x6F45, 0xDE8A, 0xAD35, 0x4A4B, 0x9496, 0x390D, 0x721A},
         {0xD849, 0xA0B3, 0x5147, 0xA28E, 0x553D, 0xAA7A, 0x44D5},
         {0x0375, 0x06EA, 0x0DD4, 0x1BA8, 0x3750, 0x6EA0, 0xDD40},
         {0x4563, 0x8AC6, 0x05AD, 0x0B5A, 0x16B4, 0x2D68, 0x5AD0},
         {0x7B61, 0xF6C2, 0xFDA5, 0xEB6B, 0xC6F7, 0x9DCF, 0x2BBF},
         {0xAEFC, 0x4DD9, 0x9BB2, 0x2745, 0x4E8A, 0x9D14, 0x2A09}
    };
    
    
    static final public String generateHash(String pwd) {
        String encryptedPwd="00000000";
        String password = pwd;
        
        
        
        if(password != null && password.length() > 0) {
            int hi=0;
            int lo=0;

            
            if(password.length() > 15) {
                password = password.substring(0,15);
            }

            
            
            hi = initialCodeArray[password.length()-1];
            
            int fidx = 0;
            int idxR = password.length()-1;
            
            
            
            for(; fidx<password.length(); fidx++,idxR--) {
                int ch = password.charAt(fidx);
                if((ch & 0x0001)!= 0) {
                    hi = hi ^ encryptionMatrix[idxR][0];
                }
                if((ch & 0x0002)!= 0) {
                    hi = hi ^ encryptionMatrix[idxR][1];
                }
                if((ch & 0x0004)!= 0) {
                    hi = hi ^ encryptionMatrix[idxR][2];
                }
                if((ch & 0x0008)!= 0) {
                    hi = hi ^ encryptionMatrix[idxR][3];
                }
                if((ch & 0x0010)!= 0) {
                    hi = hi ^ encryptionMatrix[idxR][4];
                }
                if((ch & 0x0020)!= 0) {
                    hi = hi ^ encryptionMatrix[idxR][5];
                }
                if((ch & 0x0040)!= 0) {
                    hi = hi ^ encryptionMatrix[idxR][6];
                }
            }
            
            fidx = password.length()-1;
            lo = 0;
            
            for(;fidx>= 0; fidx--) {
                int ch = password.charAt(fidx);
                lo = (((lo >> 14) & 0x001) | (( lo << 1) & 0x7fff)) ^ ch;
            }
            
            lo = (((lo >> 14) & 0x001) | (( lo << 1) & 0x7fff)) ^ password.length() ^ 0xCE4B;
            
            
            
            encryptedPwd = Integer.toHexString(lo).substring(2,4) + Integer.toHexString(lo).substring(0,2);
            encryptedPwd += Integer.toHexString(hi).substring(2,4) + Integer.toHexString(hi).substring(0,2);
        }
        return encryptedPwd;
    }
}


