

package com.lowagie.text.rtf;

import java.awt.Color;

import com.lowagie.text.Font;


public class RtfFont extends Font {
    
    private String familyName = "";

    
    public RtfFont(String familyName) {
        super(Font.UNDEFINED);
        this.familyName = familyName;
    }

    
    public RtfFont(String familyName, float size) {
        super(Font.UNDEFINED, size);
        this.familyName = familyName;
    }

    
    public RtfFont(String familyName, float size, int style) {
        super(Font.UNDEFINED, size, style);
        this.familyName = familyName;
    }

    
    public RtfFont(String familyName, float size, int style, Color color) {
        super(Font.UNDEFINED, size, style, color);
        this.familyName = familyName;
    }

    
    public String getFamilyname() {
        return this.familyName;
    }

    
    public Font difference(Font font) {
        String dFamilyname = font.getFamilyname();
        if(dFamilyname == null || dFamilyname.trim().equals("")) {
            dFamilyname = this.familyName;
        }

        float dSize = font.getSize();
        if(dSize == Font.UNDEFINED) {
            dSize = this.getSize();
        }

        int dStyle = Font.UNDEFINED;
        if(this.getStyle() != Font.UNDEFINED && font.getStyle() != Font.UNDEFINED) {
            dStyle = this.getStyle() | font.getStyle();
        } else if(this.getStyle() != Font.UNDEFINED) {
            dStyle = this.getStyle();
        } else if(font.getStyle() != Font.UNDEFINED) {
            dStyle = font.getStyle();
        }

        Color dColor = font.getColor();
        if(dColor == null) {
            dColor = this.getColor();
        }

        return new RtfFont(dFamilyname, dSize, dStyle, dColor);
    }
}
