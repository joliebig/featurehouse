

package com.lowagie.text.pdf;

import java.awt.Color;

public abstract class ExtendedColor extends Color{
    
    private static final long serialVersionUID = 2722660170712380080L;
    
    public static final int TYPE_RGB = 0;
    
    public static final int TYPE_GRAY = 1;
    
    public static final int TYPE_CMYK = 2;
    
    public static final int TYPE_SEPARATION = 3;
    
    public static final int TYPE_PATTERN = 4;
    
    public static final int TYPE_SHADING = 5;
    
    protected int type;

    
    public ExtendedColor(int type) {
        super(0, 0, 0);
        this.type = type;
    }
    
    
    public ExtendedColor(int type, float red, float green, float blue) {
        super(normalize(red), normalize(green), normalize(blue));
        this.type = type;
    }
    
    
    public int getType() {
        return type;
    }
    
    
    public static int getType(Color color) {
        if (color instanceof ExtendedColor)
            return ((ExtendedColor)color).getType();
        return TYPE_RGB;
    }

    static final float normalize(float value) {
        if (value < 0)
            return 0;
        if (value > 1)
            return 1;
        return value;
    }
}