
package com.lowagie.text.pdf;

import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;


public class SequenceList {
    protected static final int COMMA = 1;
    protected static final int MINUS = 2;
    protected static final int NOT = 3;
    protected static final int TEXT = 4;
    protected static final int NUMBER = 5;
    protected static final int END = 6;
    protected static final char EOT = '\u';

    private static final int FIRST = 0;
    private static final int DIGIT = 1;
    private static final int OTHER = 2;
    private static final int DIGIT2 = 3;
    private static final String NOT_OTHER = "-,!0123456789";

    protected char text[];
    protected int ptr;
    protected int number;
    protected String other;

    protected int low;
    protected int high;
    protected boolean odd;
    protected boolean even;
    protected boolean inverse;

    protected SequenceList(String range) {
        ptr = 0;
        text = range.toCharArray();
    }
    
    protected char nextChar() {
        while (true) {
            if (ptr >= text.length)
                return EOT;
            char c = text[ptr++];
            if (c > ' ')
                return c;
        }
    }
    
    protected void putBack() {
        --ptr;
        if (ptr < 0)
            ptr = 0;
    }
    
    protected int getType() {
        StringBuffer buf = new StringBuffer();
        int state = FIRST;
        while (true) {
            char c = nextChar();
            if (c == EOT) {
                if (state == DIGIT) {
                    number = Integer.parseInt(other = buf.toString());
                    return NUMBER;
                }
                else if (state == OTHER) {
                    other = buf.toString().toLowerCase();
                    return TEXT;
                }
                return END;
            }
            switch (state) {
                case FIRST:
                    switch (c) {
                        case '!':
                            return NOT;
                        case '-':
                            return MINUS;
                        case ',':
                            return COMMA;
                    }
                    buf.append(c);
                    if (c >= '0' && c <= '9')
                        state = DIGIT;
                    else
                        state = OTHER;
                    break;
                case DIGIT:
                    if (c >= '0' && c <= '9')
                        buf.append(c);
                    else {
                        putBack();
                        number = Integer.parseInt(other = buf.toString());
                        return NUMBER;
                    }
                    break;
                case OTHER:
                    if (NOT_OTHER.indexOf(c) < 0)
                        buf.append(c);
                    else {
                        putBack();
                        other = buf.toString().toLowerCase();
                        return TEXT;
                    }
                    break;
            }
        }
    }
    
    private void otherProc() {
        if (other.equals("odd") || other.equals("o")) {
            odd = true;
            even = false;
        }
        else if (other.equals("even") || other.equals("e")) {
            odd = false;
            even = true;
        }
    }
    
    protected boolean getAttributes() {
        low = -1;
        high = -1;
        odd = even = inverse = false;
        int state = OTHER;
        while (true) {
            int type = getType();
            if (type == END || type == COMMA) {
                if (state == DIGIT)
                    high = low;
                return (type == END);
            }
            switch (state) {
                case OTHER:
                    switch (type) {
                        case NOT:
                            inverse = true;
                            break;
                        case MINUS:
                            state = DIGIT2;
                            break;
                        default:
                            if (type == NUMBER) {
                                low = number;
                                state = DIGIT;
                            }
                            else
                                otherProc();
                            break;
                    }
                    break;
                case DIGIT:
                    switch (type) {
                        case NOT:
                            inverse = true;
                            state = OTHER;
                            high = low;
                            break;
                        case MINUS:
                            state = DIGIT2;
                            break;
                        default:
                            high = low;
                            state = OTHER;
                            otherProc();
                            break;
                    }
                    break;
                case DIGIT2:
                    switch (type) {
                        case NOT:
                            inverse = true;
                            state = OTHER;
                            break;
                        case MINUS:
                            break;
                        case NUMBER:
                            high = number;
                            state = OTHER;
                            break;
                        default:
                            state = OTHER;
                            otherProc();
                            break;
                    }
                    break;
            }
        }
    }
    
        
    public static List expand(String ranges, int maxNumber) {
        SequenceList parse = new SequenceList(ranges);
        LinkedList list = new LinkedList();
        boolean sair = false;
        while (!sair) {
            sair = parse.getAttributes();
            if (parse.low == -1 && parse.high == -1 && !parse.even && !parse.odd)
                continue;
            if (parse.low < 1)
                parse.low = 1;
            if (parse.high < 1 || parse.high > maxNumber)
                parse.high = maxNumber;
            if (parse.low > maxNumber)
                parse.low = maxNumber;
            
            
            int inc = 1;
            if (parse.inverse) {
                if (parse.low > parse.high) {
                    int t = parse.low;
                    parse.low = parse.high;
                    parse.high = t;
                }
                for (ListIterator it = list.listIterator(); it.hasNext();) {
                    int n = ((Integer)it.next()).intValue();
                    if (parse.even && (n & 1) == 1)
                        continue;
                    if (parse.odd && (n & 1) == 0)
                        continue;
                    if (n >= parse.low && n <= parse.high)
                        it.remove();
                }
            }
            else {
                if (parse.low > parse.high) {
                    inc = -1;
                    if (parse.odd || parse.even) {
                        --inc;
                        if (parse.even)
                            parse.low &= ~1;
                        else
                            parse.low -= ((parse.low & 1) == 1 ? 0 : 1);
                    }
                    for (int k = parse.low; k >= parse.high; k += inc)
                        list.add(new Integer(k));
                }
                else {
                    if (parse.odd || parse.even) {
                        ++inc;
                        if (parse.odd)
                            parse.low |= 1;
                        else
                            parse.low += ((parse.low & 1) == 1 ? 1 : 0);
                    }
                    for (int k = parse.low; k <= parse.high; k += inc) {
                        list.add(new Integer(k));
                    }
                }
            }



        }
        return list;
    }
}