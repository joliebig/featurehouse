

package com.lowagie.text.pdf;


public class CMYKColor extends ExtendedColor {

    private static final long serialVersionUID = 5940378778276468452L;
    float cyan;
    float magenta;
    float yellow;
    float black;

    
    public CMYKColor(int intCyan, int intMagenta, int intYellow, int intBlack) {
        this((float)intCyan / 255f, (float)intMagenta / 255f, (float)intYellow / 255f, (float)intBlack / 255f);
    }

    
    public CMYKColor(float floatCyan, float floatMagenta, float floatYellow, float floatBlack) {
        super(TYPE_CMYK, 1f - floatCyan - floatBlack, 1f - floatMagenta - floatBlack, 1f - floatYellow - floatBlack);
        cyan = normalize(floatCyan);
        magenta = normalize(floatMagenta);
        yellow = normalize(floatYellow);
        black = normalize(floatBlack);
    }
    
    
    public float getCyan() {
        return cyan;
    }

    
    public float getMagenta() {
        return magenta;
    }

    
    public float getYellow() {
        return yellow;
    }

    
    public float getBlack() {
        return black;
    }

    public boolean equals(Object obj) {
        if (!(obj instanceof CMYKColor))
            return false;
        CMYKColor c2 = (CMYKColor)obj;
        return (cyan == c2.cyan && magenta == c2.magenta && yellow == c2.yellow && black == c2.black);
    }
    
    public int hashCode() {
        return Float.floatToIntBits(cyan) ^ Float.floatToIntBits(magenta) ^ Float.floatToIntBits(yellow) ^ Float.floatToIntBits(black); 
    }
    
}
