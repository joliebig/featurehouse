class Foo {
    public void bar1() {}
    public void spam2() {
    	int a;
    	int b;
    }
    public void bar2() {}
    class Bar  implements Y {
    	int y;
    	int a;
    	int b;
    	protected int q;
    }
    public void setValues() {
        colorCodes.setSelected(_prefs.getBoolean("tableColorCodesOn"));
        //antialias.setSelected(_prefs.getBoolean("antialias"));
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
