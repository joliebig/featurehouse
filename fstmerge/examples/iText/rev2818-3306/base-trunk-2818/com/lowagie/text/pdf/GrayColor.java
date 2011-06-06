

package com.lowagie.text.pdf;


public class GrayColor extends ExtendedColor {

    private static final long serialVersionUID = -6571835680819282746L;

    private float gray;
    
    public static final GrayColor GRAYBLACK = new GrayColor(0f);
    public static final GrayColor GRAYWHITE = new GrayColor(1f);

    public GrayColor(int intGray) {
        this((float)intGray / 255f);
    }

    public GrayColor(float floatGray) {
        super(TYPE_GRAY, floatGray, floatGray, floatGray);
        gray = normalize(floatGray);
    }
    
    public float getGray() {
        return gray;
    }

    public boolean equals(Object obj) {
        return obj instanceof GrayColor && ((GrayColor)obj).gray == this.gray;
    }
    
    public int hashCode() {
        return Float.floatToIntBits(gray);
    }
    
}
