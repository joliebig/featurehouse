class Foo implements A {
	int gggggggggggggg;
    public void bar1() {}
    public void bar2() {}
    public class Bar implements B {
    	private int x;
    	protected int y;
    	public int q;
    }
    public void setValues() {
        colorCodes.setSelected(_prefs.getBoolean("tableColorCodesOn"));
        antialias.setSelected(_prefs.getBoolean("antialias"));
        fontSize.setText("" + _prefs.getInt("menuFontSize"));
        oldMenuFontSize = _prefs.getInt("menuFontSize");
        overrideFonts.setSelected(_prefs.getBoolean("overrideDefaultFonts"));
        oldOverrideFontSize = overrideFonts.isSelected();
        fontSize.setEnabled(overrideFonts.isSelected());
        //useCustomIconTheme.setSelected(_prefs.getBoolean("useCustomIconTheme"));
        //customIconThemeFile.setText(_prefs.get("customIconThemeFile"));
        colorPanel.setValues();
    }
}
