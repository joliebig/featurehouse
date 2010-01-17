class Foo {
	String s = "#";
    public void bar1() {}
    public void spam1() {}
    public void bar2() {}
    public void spam2() {
    	int x;
    	int y;
    	@SuppressWarnings("unchecked")
    	for (ExportFormatTemplateExtension e : plugin.getExportFormatTemplateExtensions()){}
    }
    private class Bar implements X {
    	int x;
    	int a;
    	private int q;
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