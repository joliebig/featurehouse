

package com.lowagie.text.pdf;


public class SpotColor extends ExtendedColor {

    private static final long serialVersionUID = -6257004582113248079L;
    PdfSpotColor spot;
    float tint;

    public SpotColor(PdfSpotColor spot, float tint) {
        super(TYPE_SEPARATION,
            (spot.getAlternativeCS().getRed() / 255f - 1f) * tint + 1,
            (spot.getAlternativeCS().getGreen() / 255f - 1f) * tint + 1,
            (spot.getAlternativeCS().getBlue() / 255f - 1f) * tint + 1);
        this.spot = spot;
        this.tint = tint;
    }
    
    public SpotColor(PdfSpotColor spot) {
        this(spot, spot.getTint());
    }
    
    public PdfSpotColor getPdfSpotColor() {
        return spot;
    }
    
    public float getTint() {
        return tint;
    }

    public boolean equals(Object obj) {
        return this == obj;
    }
    
    public int hashCode() {
        return spot.hashCode() ^ Float.floatToIntBits(tint);
    }
}
