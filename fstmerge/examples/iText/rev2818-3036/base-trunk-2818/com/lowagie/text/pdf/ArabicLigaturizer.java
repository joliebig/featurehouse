
package com.lowagie.text.pdf;


public class ArabicLigaturizer {
    
    static boolean isVowel(char s) {
        return ((s >= 0x064B) && (s <= 0x0655)) || (s == 0x0670);
    }

    static char charshape(char s, int which)
    
    {
        int l, r, m;
        if ((s >= 0x0621) && (s <= 0x06D3)) {
            l = 0;
            r = chartable.length - 1;
            while (l <= r) {
                m = (l + r) / 2;
                if (s == chartable[m][0]) {
                    return chartable[m][which + 1];
                }
                else if (s < chartable[m][0]) {
                    r = m - 1;
                }
                else {
                    l = m + 1;
                }
            }
        }
        else if (s >= 0xfef5 && s <= 0xfefb)
            return (char)(s + which);
        return s;
    }

    static int shapecount(char s) {
        int l, r, m;
        if ((s >= 0x0621) && (s <= 0x06D3) && !isVowel(s)) {
            l = 0;
            r = chartable.length - 1;
            while (l <= r) {
                m = (l + r) / 2;
                if (s == chartable[m][0]) {
                    return chartable[m].length - 1;
                }
                else if (s < chartable[m][0]) {
                    r = m - 1;
                }
                else {
                    l = m + 1;
                }
            }
        }
        else if (s == ZWJ) {
            return 4;
        }
        return 1;
    }
    
    static int ligature(char newchar, charstruct oldchar) {
    
        int retval = 0;
        
        if (oldchar.basechar == 0)
            return 0;
        if (isVowel(newchar)) {
            retval = 1;
            if ((oldchar.vowel != 0) && (newchar != SHADDA)) {
                retval = 2;           
            }
            switch (newchar) {
                case SHADDA:
                    if (oldchar.mark1 == 0) {
                        oldchar.mark1 = SHADDA;
                    }
                    else {
                        return 0;         
                    }
                    break;
                case HAMZABELOW:
                    switch (oldchar.basechar) {
                        case ALEF:
                            oldchar.basechar = ALEFHAMZABELOW;
                            retval = 2;
                            break;
                        case LAM_ALEF:
                            oldchar.basechar = LAM_ALEFHAMZABELOW;
                            retval = 2;
                            break;
                        default:
                            oldchar.mark1 = HAMZABELOW;
                            break;
                    }
                    break;
                case HAMZAABOVE:
                    switch (oldchar.basechar) {
                        case ALEF:
                            oldchar.basechar = ALEFHAMZA;
                            retval = 2;
                            break;
                        case LAM_ALEF:
                            oldchar.basechar = LAM_ALEFHAMZA;
                            retval = 2;
                            break;
                        case WAW:
                            oldchar.basechar = WAWHAMZA;
                            retval = 2;
                            break;
                        case YEH:
                        case ALEFMAKSURA:
                        case FARSIYEH:
                            oldchar.basechar = YEHHAMZA;
                            retval = 2;
                            break;
                        default:           
                            oldchar.mark1 = HAMZAABOVE;
                            break;
                    }
                    break;
                case MADDA:
                    switch (oldchar.basechar) {
                        case ALEF:
                            oldchar.basechar = ALEFMADDA;
                            retval = 2;
                            break;
                    }
                    break;
                default:
                    oldchar.vowel = newchar;
                    break;
            }
            if (retval == 1) {
                oldchar.lignum++;
            }
            return retval;
        }
        if (oldchar.vowel != 0) {  
            return 0;
        }
        
        switch (oldchar.basechar) {
            case LAM:
                switch (newchar) {
                    case ALEF:
                        oldchar.basechar = LAM_ALEF;
                        oldchar.numshapes = 2;
                        retval = 3;
                        break;
                    case ALEFHAMZA:
                        oldchar.basechar = LAM_ALEFHAMZA;
                        oldchar.numshapes = 2;
                        retval = 3;
                        break;
                    case ALEFHAMZABELOW:
                        oldchar.basechar = LAM_ALEFHAMZABELOW;
                        oldchar.numshapes = 2;
                        retval = 3;
                        break;
                    case ALEFMADDA:
                        oldchar.basechar = LAM_ALEFMADDA;
                        oldchar.numshapes = 2;
                        retval = 3;
                        break;
                }
                break;
            case 0:
                oldchar.basechar = newchar;
                oldchar.numshapes = shapecount(newchar);
                retval = 1;
                break;
        }
        return retval;
    }
    
    static void copycstostring(StringBuffer string, charstruct s, int level) {
    
        if (s.basechar == 0)
            return;
        
        string.append(s.basechar);
        s.lignum--;
        if (s.mark1 != 0) {
            if ((level & ar_novowel) == 0) {
                string.append(s.mark1);
                s.lignum--;
            }
            else {
                s.lignum--;
            }
        }
        if (s.vowel != 0) {
            if ((level & ar_novowel) == 0) {
                string.append(s.vowel);
                s.lignum--;
            }
            else {                       
                s.lignum--;
            }
        }






    }

    
    static void doublelig(StringBuffer string, int level)
    
    {
        int len;
        int olen = len = string.length();
        int j = 0, si = 1;
        char lapresult;
        
        while (si < olen) {
            lapresult = 0;
            if ((level & ar_composedtashkeel) != 0) {
                switch (string.charAt(j)) {
                    case SHADDA:
                        switch (string.charAt(si)) {
                            case KASRA:
                                lapresult = 0xFC62;
                                break;
                            case FATHA:
                                lapresult = 0xFC60;
                                break;
                            case DAMMA:
                                lapresult = 0xFC61;
                                break;
                            case 0x064C:
                                lapresult = 0xFC5E;
                                break;
                            case 0x064D:
                                lapresult = 0xFC5F;
                                break;
                        }
                        break;
                    case KASRA:
                        if (string.charAt(si) == SHADDA)
                            lapresult = 0xFC62;
                        break;
                    case FATHA:
                        if (string.charAt(si) == SHADDA)
                            lapresult = 0xFC60;
                        break;
                    case DAMMA:
                        if (string.charAt(si) == SHADDA)
                            lapresult = 0xFC61;
                        break;
                }
            }
            
            if ((level & ar_lig) != 0) {
                switch (string.charAt(j)) {
                    case 0xFEDF:       
                        switch (string.charAt(si)) {
                            case 0xFE9E:
                                lapresult = 0xFC3F;
                                break;        
                            case 0xFEA0:
                                lapresult = 0xFCC9;
                                break;        
                            case 0xFEA2:
                                lapresult = 0xFC40;
                                break;        
                            case 0xFEA4:
                                lapresult = 0xFCCA;
                                break;        
                            case 0xFEA6:
                                lapresult = 0xFC41;
                                break;        
                            case 0xFEA8:
                                lapresult = 0xFCCB;
                                break;        
                            case 0xFEE2:
                                lapresult = 0xFC42;
                                break;        
                            case 0xFEE4:
                                lapresult = 0xFCCC;
                                break;        
                        }
                        break;
                    case 0xFE97:       
                        switch (string.charAt(si)) {
                            case 0xFEA0:
                                lapresult = 0xFCA1;
                                break;        
                            case 0xFEA4:
                                lapresult = 0xFCA2;
                                break;        
                            case 0xFEA8:
                                lapresult = 0xFCA3;
                                break;        
                        }
                        break;
                    case 0xFE91:       
                        switch (string.charAt(si)) {
                            case 0xFEA0:
                                lapresult = 0xFC9C;
                                break;        
                            case 0xFEA4:
                                lapresult = 0xFC9D;
                                break;        
                            case 0xFEA8:
                                lapresult = 0xFC9E;
                                break;        
                        }
                        break;
                    case 0xFEE7:       
                        switch (string.charAt(si)) {
                            case 0xFEA0:
                                lapresult = 0xFCD2;
                                break;        
                            case 0xFEA4:
                                lapresult = 0xFCD3;
                                break;        
                            case 0xFEA8:
                                lapresult = 0xFCD4;
                                break;        
                        }
                        break;
                        
                    case 0xFEE8:       
                        switch (string.charAt(si)) {
                            case 0xFEAE:
                                lapresult = 0xFC8A;
                                break;        
                            case 0xFEB0:
                                lapresult = 0xFC8B;
                                break;        
                        }
                        break;
                    case 0xFEE3:       
                        switch (string.charAt(si)) {
                            case 0xFEA0:
                                lapresult = 0xFCCE;
                                break;        
                            case 0xFEA4:
                                lapresult = 0xFCCF;
                                break;        
                            case 0xFEA8:
                                lapresult = 0xFCD0;
                                break;        
                            case 0xFEE4:
                                lapresult = 0xFCD1;
                                break;        
                        }
                        break;
                        
                    case 0xFED3:       
                        switch (string.charAt(si)) {
                            case 0xFEF2:
                                lapresult = 0xFC32;
                                break;        
                        }
                        break;
                        
                    default:
                        break;
                }                   
            }
            if (lapresult != 0) {
                string.setCharAt(j, lapresult);
                len--;
                si++;                 
                
            }
            else {
                j++;
                string.setCharAt(j, string.charAt(si));
                si++;
            }
        }
        string.setLength(len);
    }

    static boolean connects_to_left(charstruct a) {
        return a.numshapes > 2;
    }
    
    static void shape(char text[], StringBuffer string, int level) {
  
        int join;
        int which;
        char nextletter;
        
        int p = 0;                     
        charstruct oldchar = new charstruct();
        charstruct curchar = new charstruct();
        while (p < text.length) {
            nextletter = text[p++];
            
            
            join = ligature(nextletter, curchar);
            if (join == 0) {                       
                int nc = shapecount(nextletter);
                
                if (nc == 1) {
                    which = 0;        
                }
                else {
                    which = 2;        
                }
                if (connects_to_left(oldchar)) {
                    which++;
                }
                
                which = which % (curchar.numshapes);
                curchar.basechar = charshape(curchar.basechar, which);
                
                
                copycstostring(string, oldchar, level);
                oldchar = curchar;    
                
                
                curchar = new charstruct();
                curchar.basechar = nextletter;
                curchar.numshapes = nc;
                curchar.lignum++;
                
            }
            else if (join == 1) {
            }
            
            
            
            
            
        }
        
        
        if (connects_to_left(oldchar))
            which = 1;
        else
            which = 0;
        which = which % (curchar.numshapes);
        curchar.basechar = charshape(curchar.basechar, which);
        
        
        copycstostring(string, oldchar, level);
        copycstostring(string, curchar, level);
    }

    static int arabic_shape(char src[], int srcoffset, int srclength, char dest[], int destoffset, int destlength, int level) {
        char str[] = new char[srclength];
        for (int k = srclength + srcoffset - 1; k >= srcoffset; --k)
            str[k - srcoffset] = src[k];
        StringBuffer string = new StringBuffer(srclength);
        shape(str, string, level);
        if ((level & (ar_composedtashkeel | ar_lig)) != 0)
            doublelig(string, level);

        System.arraycopy(string.toString().toCharArray(), 0, dest, destoffset, string.length());
        return string.length();
    }

    static void processNumbers(char text[], int offset, int length, int options) {
        int limit = offset + length;
        if ((options & DIGITS_MASK) != 0) {
            char digitBase = '\u'; 
            switch (options & DIGIT_TYPE_MASK) {
                case DIGIT_TYPE_AN:
                    digitBase = '\u';  
                    break;
                    
                case DIGIT_TYPE_AN_EXTENDED:
                    digitBase = '\u';  
                    break;
                    
                default:
                    break;
            }
            
            switch (options & DIGITS_MASK) {
                case DIGITS_EN2AN: {
                    int digitDelta = digitBase - '\u';
                    for (int i = offset; i < limit; ++i) {
                        char ch = text[i];
                        if (ch <= '\u' && ch >= '\u') {
                            text[i] += digitDelta;
                        }
                    }
                }
                break;
                
                case DIGITS_AN2EN: {
                    char digitTop = (char)(digitBase + 9);
                    int digitDelta = '\u' - digitBase;
                    for (int i = offset; i < limit; ++i) {
                        char ch = text[i];
                        if (ch <= digitTop && ch >= digitBase) {
                            text[i] += digitDelta;
                        }
                    }
                }
                break;
                
                case DIGITS_EN2AN_INIT_LR:
                    shapeToArabicDigitsWithContext(text, 0, length, digitBase, false);
                    break;
                    
                case DIGITS_EN2AN_INIT_AL:
                    shapeToArabicDigitsWithContext(text, 0, length, digitBase, true);
                    break;
                    
                default:
                    break;
            }
        }
    }
    
    static void shapeToArabicDigitsWithContext(char[] dest, int start, int length, char digitBase,  boolean lastStrongWasAL) {
        digitBase -= '0'; 
 
        int limit = start + length;
        for(int i = start; i < limit; ++i) {
            char ch = dest[i];
            switch (BidiOrder.getDirection(ch)) {
            case BidiOrder.L:
            case BidiOrder.R:
                lastStrongWasAL = false;
                break;
            case BidiOrder.AL:
                lastStrongWasAL = true;
                break;
            case BidiOrder.EN:
                if (lastStrongWasAL && ch <= '\u') {
                    dest[i] = (char)(ch + digitBase);
                }
                break;
            default:
                break;
            }
        }
    }

    private static final char ALEF = 0x0627;
    private static final char ALEFHAMZA = 0x0623;
    private static final char ALEFHAMZABELOW = 0x0625;
    private static final char ALEFMADDA = 0x0622;
    private static final char LAM = 0x0644;
    private static final char HAMZA = 0x0621;
    private static final char TATWEEL = 0x0640;
    private static final char ZWJ = 0x200D;

    private static final char HAMZAABOVE = 0x0654;
    private static final char HAMZABELOW = 0x0655;

    private static final char WAWHAMZA = 0x0624;
    private static final char YEHHAMZA = 0x0626;
    private static final char WAW = 0x0648;
    private static final char ALEFMAKSURA = 0x0649;
    private static final char YEH = 0x064A;
    private static final char FARSIYEH = 0x06CC;

    private static final char SHADDA = 0x0651;
    private static final char KASRA = 0x0650;
    private static final char FATHA = 0x064E;
    private static final char DAMMA = 0x064F;
    private static final char MADDA = 0x0653;

    private static final char LAM_ALEF = 0xFEFB;
    private static final char LAM_ALEFHAMZA = 0xFEF7;
    private static final char LAM_ALEFHAMZABELOW = 0xFEF9;
    private static final char LAM_ALEFMADDA = 0xFEF5;

    private static final char chartable[][] = {
        {0x0621, 0xFE80}, 
        {0x0622, 0xFE81, 0xFE82}, 
        {0x0623, 0xFE83, 0xFE84}, 
        {0x0624, 0xFE85, 0xFE86}, 
        {0x0625, 0xFE87, 0xFE88}, 
        {0x0626, 0xFE89, 0xFE8A, 0xFE8B, 0xFE8C}, 
        {0x0627, 0xFE8D, 0xFE8E}, 
        {0x0628, 0xFE8F, 0xFE90, 0xFE91, 0xFE92}, 
        {0x0629, 0xFE93, 0xFE94}, 
        {0x062A, 0xFE95, 0xFE96, 0xFE97, 0xFE98}, 
        {0x062B, 0xFE99, 0xFE9A, 0xFE9B, 0xFE9C}, 
        {0x062C, 0xFE9D, 0xFE9E, 0xFE9F, 0xFEA0}, 
        {0x062D, 0xFEA1, 0xFEA2, 0xFEA3, 0xFEA4}, 
        {0x062E, 0xFEA5, 0xFEA6, 0xFEA7, 0xFEA8}, 
        {0x062F, 0xFEA9, 0xFEAA}, 
        {0x0630, 0xFEAB, 0xFEAC}, 
        {0x0631, 0xFEAD, 0xFEAE}, 
        {0x0632, 0xFEAF, 0xFEB0}, 
        {0x0633, 0xFEB1, 0xFEB2, 0xFEB3, 0xFEB4}, 
        {0x0634, 0xFEB5, 0xFEB6, 0xFEB7, 0xFEB8}, 
        {0x0635, 0xFEB9, 0xFEBA, 0xFEBB, 0xFEBC}, 
        {0x0636, 0xFEBD, 0xFEBE, 0xFEBF, 0xFEC0}, 
        {0x0637, 0xFEC1, 0xFEC2, 0xFEC3, 0xFEC4}, 
        {0x0638, 0xFEC5, 0xFEC6, 0xFEC7, 0xFEC8}, 
        {0x0639, 0xFEC9, 0xFECA, 0xFECB, 0xFECC}, 
        {0x063A, 0xFECD, 0xFECE, 0xFECF, 0xFED0}, 
        {0x0640, 0x0640, 0x0640, 0x0640, 0x0640}, 
        {0x0641, 0xFED1, 0xFED2, 0xFED3, 0xFED4}, 
        {0x0642, 0xFED5, 0xFED6, 0xFED7, 0xFED8}, 
        {0x0643, 0xFED9, 0xFEDA, 0xFEDB, 0xFEDC}, 
        {0x0644, 0xFEDD, 0xFEDE, 0xFEDF, 0xFEE0}, 
        {0x0645, 0xFEE1, 0xFEE2, 0xFEE3, 0xFEE4}, 
        {0x0646, 0xFEE5, 0xFEE6, 0xFEE7, 0xFEE8}, 
        {0x0647, 0xFEE9, 0xFEEA, 0xFEEB, 0xFEEC}, 
        {0x0648, 0xFEED, 0xFEEE}, 
        {0x0649, 0xFEEF, 0xFEF0, 0xFBE8, 0xFBE9}, 
        {0x064A, 0xFEF1, 0xFEF2, 0xFEF3, 0xFEF4}, 
        {0x0671, 0xFB50, 0xFB51}, 
        {0x0679, 0xFB66, 0xFB67, 0xFB68, 0xFB69}, 
        {0x067A, 0xFB5E, 0xFB5F, 0xFB60, 0xFB61}, 
        {0x067B, 0xFB52, 0xFB53, 0xFB54, 0xFB55}, 
        {0x067E, 0xFB56, 0xFB57, 0xFB58, 0xFB59}, 
        {0x067F, 0xFB62, 0xFB63, 0xFB64, 0xFB65}, 
        {0x0680, 0xFB5A, 0xFB5B, 0xFB5C, 0xFB5D}, 
        {0x0683, 0xFB76, 0xFB77, 0xFB78, 0xFB79}, 
        {0x0684, 0xFB72, 0xFB73, 0xFB74, 0xFB75}, 
        {0x0686, 0xFB7A, 0xFB7B, 0xFB7C, 0xFB7D}, 
        {0x0687, 0xFB7E, 0xFB7F, 0xFB80, 0xFB81}, 
        {0x0688, 0xFB88, 0xFB89}, 
        {0x068C, 0xFB84, 0xFB85}, 
        {0x068D, 0xFB82, 0xFB83}, 
        {0x068E, 0xFB86, 0xFB87}, 
        {0x0691, 0xFB8C, 0xFB8D}, 
        {0x0698, 0xFB8A, 0xFB8B}, 
        {0x06A4, 0xFB6A, 0xFB6B, 0xFB6C, 0xFB6D}, 
        {0x06A6, 0xFB6E, 0xFB6F, 0xFB70, 0xFB71}, 
        {0x06A9, 0xFB8E, 0xFB8F, 0xFB90, 0xFB91}, 
        {0x06AD, 0xFBD3, 0xFBD4, 0xFBD5, 0xFBD6}, 
        {0x06AF, 0xFB92, 0xFB93, 0xFB94, 0xFB95}, 
        {0x06B1, 0xFB9A, 0xFB9B, 0xFB9C, 0xFB9D}, 
        {0x06B3, 0xFB96, 0xFB97, 0xFB98, 0xFB99}, 
        {0x06BA, 0xFB9E, 0xFB9F}, 
        {0x06BB, 0xFBA0, 0xFBA1, 0xFBA2, 0xFBA3}, 
        {0x06BE, 0xFBAA, 0xFBAB, 0xFBAC, 0xFBAD}, 
        {0x06C0, 0xFBA4, 0xFBA5}, 
        {0x06C1, 0xFBA6, 0xFBA7, 0xFBA8, 0xFBA9}, 
        {0x06C5, 0xFBE0, 0xFBE1}, 
        {0x06C6, 0xFBD9, 0xFBDA}, 
        {0x06C7, 0xFBD7, 0xFBD8}, 
        {0x06C8, 0xFBDB, 0xFBDC}, 
        {0x06C9, 0xFBE2, 0xFBE3}, 
        {0x06CB, 0xFBDE, 0xFBDF}, 
        {0x06CC, 0xFBFC, 0xFBFD, 0xFBFE, 0xFBFF}, 
        {0x06D0, 0xFBE4, 0xFBE5, 0xFBE6, 0xFBE7}, 
        {0x06D2, 0xFBAE, 0xFBAF}, 
        {0x06D3, 0xFBB0, 0xFBB1} 
        };

        public static final int ar_nothing  = 0x0;
        public static final int ar_novowel = 0x1;
        public static final int ar_composedtashkeel = 0x4;
        public static final int ar_lig = 0x8;
        
        public static final int DIGITS_EN2AN = 0x20;
        
        
        public static final int DIGITS_AN2EN = 0x40;
        
        
        public static final int DIGITS_EN2AN_INIT_LR = 0x60;
        
        
        public static final int DIGITS_EN2AN_INIT_AL = 0x80;
        
        
        private static final int DIGITS_RESERVED = 0xa0;
        
        
        public static final int DIGITS_MASK = 0xe0;
        
        
        public static final int DIGIT_TYPE_AN = 0;
        
        
        public static final int DIGIT_TYPE_AN_EXTENDED = 0x100;

        
        public static final int DIGIT_TYPE_MASK = 0x0100; 

        static class charstruct {
            char basechar;
            char mark1;               
            char vowel;
            int lignum;           
            int numshapes = 1;
        };


}
