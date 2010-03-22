package com.horstmann.violet.framework;

public class EditorFrame {

    protected ResourceBundle editorResources;
    protected ResourceFactory factory;
    protected ResourceBundle appResources;
    protected JMenuBar menuBar;
    protected String appClassName;

    EditorFrame(Class appClass) {

    appClassName = appClass.getName();
    appResources = ResourceBundle.getBundle(appClassName + "Strings");

    editorResources = ResourceBundle.getBundle("com.horstmann.violet.framework.EditorStrings");
    factory = new ResourceFactory(editorResources);

    menuBar = new JMenuBar();
    setJMenuBar(menuBar);
    }

   // Function Hooks    
   public void savePreferences()
   {
   }   

   void SaveLookAndFeelPreferences(String laf)
   {
   }
   
   public void changeLookAndFeel(String lafName)
   {
   }
   
   protected void setTitle()
   {
   }
}