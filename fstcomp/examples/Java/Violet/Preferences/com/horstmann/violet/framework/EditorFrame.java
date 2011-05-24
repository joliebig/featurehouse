package com.horstmann.violet.framework;

public class EditorFrame {

  public PreferencesService preferences;
  
  private int maxRecentFiles = DEFAULT_MAX_RECENT_FILES;
  private static final int DEFAULT_MAX_RECENT_FILES = 5;  

  EditorFrame(Class appClass) {
      preferences = PreferencesService.getInstance(appClass);
      
      recentFiles = new ArrayList(); 
      File lastDir = new File(".");
      if (!preferences.isWebStart())
      {
         String recent = preferences.get("recent", "");
         if (recent.length() > 0)
            recentFiles.addAll(Arrays.asList(recent.split("[|]")));
         
         if (recentFiles.size() > 0)
            lastDir = new File((String) recentFiles.get(0)).getParentFile();
      }
      fileService = FileService.getInstance(lastDir);      
  }
  
    /**
    * Changes the look and feel
    * @param lafName the name of the new look and feel
    */   
   public void changeLookAndFeel(String lafName)
   {
   		// Just a function hook
   		original(lafName);
   }
   
   /**
    * Saves the user preferences before exiting.
    */
   public void savePreferences()
   {
      String recent = "";     
      for (int i = 0; i < Math.min(recentFiles.size(), maxRecentFiles); i++)
      {
         if (recent.length() > 0) recent += "|";
         recent += recentFiles.get(i);
      }      
      preferences.put("recent", recent);   
   }
 
   void SaveLookAndFeelPreferences(String laf)
   {
		preferences.put("laf", laf);
   }
  
}