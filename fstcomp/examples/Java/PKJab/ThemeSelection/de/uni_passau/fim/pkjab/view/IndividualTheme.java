
import java.awt.Color;
import java.awt.Font;

class IndividualTheme extends AbstractTheme {
    
    private Font font;
    
    private Color background;
    
    private Color foreground;

    IndividualTheme(Color background, Color foreground, Font font) {
        this.font = font;
        this.background = background;
        this.foreground = foreground;
    }

    public Color getBackgroundColor() {
        return background;
    }

    public Font getFont() {
        return font;
    }

    public Color getForegroundColor() {
        return foreground;
    }

}