package de.uni_passau.fim.pkjab.view;

import java.awt.Color;
import java.awt.Font;

public class DefaultTheme extends AbstractTheme {
	
	private Font font = new Font("Default", Font.PLAIN, 12);

	public Color getBackgroundColor() {
		return Color.BLACK;
	}

	public Font getFont() {
		return font;
	}

	public Color getForegroundColor() {
		return Color.WHITE;
	}

}
