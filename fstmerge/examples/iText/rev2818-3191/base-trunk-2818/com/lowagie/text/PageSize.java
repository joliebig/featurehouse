

package com.lowagie.text;

import java.lang.reflect.Field;



public class PageSize {
    
    
    
    
    public static final Rectangle LETTER = new Rectangle(612,792);
    
    
    public static final Rectangle NOTE = new Rectangle(540,720);
    
    
    public static final Rectangle LEGAL = new Rectangle(612,1008);
    
    
    public static final Rectangle TABLOID = new Rectangle(792,1224);

    
    public static final Rectangle EXECUTIVE = new Rectangle(522,756);

    
    public static final Rectangle POSTCARD = new Rectangle(283,416);
    
    
    public static final Rectangle A0 = new Rectangle(2384,3370);
    
    
    public static final Rectangle A1 = new Rectangle(1684,2384);
    
    
    public static final Rectangle A2 = new Rectangle(1191,1684);
    
    
    public static final Rectangle A3 = new Rectangle(842,1191);
    
    
    public static final Rectangle A4 = new Rectangle(595,842);
    
    
    public static final Rectangle A5 = new Rectangle(420,595);
    
    
    public static final Rectangle A6 = new Rectangle(297,420);
    
    
    public static final Rectangle A7 = new Rectangle(210,297);
    
    
    public static final Rectangle A8 = new Rectangle(148,210);
    
    
    public static final Rectangle A9 = new Rectangle(105,148);
    
    
    public static final Rectangle A10 = new Rectangle(73,105);
    
    
    public static final Rectangle B0 = new Rectangle(2834,4008);
    
    
    public static final Rectangle B1 = new Rectangle(2004,2834);
    
    
    public static final Rectangle B2 = new Rectangle(1417,2004);
    
    
    public static final Rectangle B3 = new Rectangle(1000,1417);
    
    
    public static final Rectangle B4 = new Rectangle(708,1000);
    
    
    public static final Rectangle B5 = new Rectangle(498,708);

    
    public static final Rectangle B6 = new Rectangle(354,498);
    
    
    public static final Rectangle B7 = new Rectangle(249,354);
    
    
    public static final Rectangle B8 = new Rectangle(175,249);

    
    public static final Rectangle B9 = new Rectangle(124,175);

    
    public static final Rectangle B10 = new Rectangle(87,124);
    
    
    public static final Rectangle ARCH_E = new Rectangle(2592,3456);
    
    
    public static final Rectangle ARCH_D = new Rectangle(1728,2592);
    
    
    public static final Rectangle ARCH_C = new Rectangle(1296,1728);
    
    
    public static final Rectangle ARCH_B = new Rectangle(864,1296);
    
    
    public static final Rectangle ARCH_A = new Rectangle(648,864);
    
    
    public static final Rectangle FLSA = new Rectangle(612,936);
    
    
    public static final Rectangle FLSE = new Rectangle(648,936);
    
    
    public static final Rectangle HALFLETTER = new Rectangle(396,612);
    
    
    public static final Rectangle _11X17 = new Rectangle(792,1224);
    
    
    public static final Rectangle ID_1 = new Rectangle(242.65f,153);
    
    
    public static final Rectangle ID_2 = new Rectangle(297,210);
    
    
    public static final Rectangle ID_3 = new Rectangle(354,249);
    
    
    public static final Rectangle LEDGER = new Rectangle(1224,792);
    
    
    public static final Rectangle CROWN_QUARTO = new Rectangle(535,697);

    
    public static final Rectangle LARGE_CROWN_QUARTO = new Rectangle(569,731);
    
    
    public static final Rectangle DEMY_QUARTO = new Rectangle(620,782);
    
    
    public static final Rectangle ROYAL_QUARTO = new Rectangle(671,884);
    
    
    public static final Rectangle CROWN_OCTAVO = new Rectangle(348,527);
    
    
    public static final Rectangle LARGE_CROWN_OCTAVO = new Rectangle(365,561);
    
    
    public static final Rectangle DEMY_OCTAVO = new Rectangle(391,612);
    
    
    public static final Rectangle ROYAL_OCTAVO = new Rectangle(442,663);
    
    
    public static final Rectangle SMALL_PAPERBACK = new Rectangle(314,504);
    
    
    public static final Rectangle PENGUIN_SMALL_PAPERBACK = new Rectangle(314,513);
    
    
    public static final Rectangle PENGUIN_LARGE_PAPERBACK = new Rectangle(365,561);
      
    
    public static Rectangle getRectangle(String name)  {
        name = name.trim().toUpperCase();
        int pos = name.indexOf(' ');
        if (pos == -1) {
            try {            
                Field field = PageSize.class.getDeclaredField(name.toUpperCase());
                return (Rectangle) field.get(null);
            } catch (Exception e) {
                throw new RuntimeException("Can't find page size " + name);          
            }
        }
        else {
            try {
                String width = name.substring(0, pos);
                String height = name.substring(pos + 1);
                return new Rectangle(Float.parseFloat(width), Float.parseFloat(height));
            } catch(Exception e) {
                throw new RuntimeException(name + " is not a valid page size format; " + e.getMessage());
            }
        }
    }
}