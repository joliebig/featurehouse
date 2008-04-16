package com.horstmann.violet.framework;

import java.util.prefs.Preferences;

/**
 * The default preferences service that uses the java.util.prefs API. 
 */
class DefaultPreferencesService extends PreferencesService
{
   
   /**
    * Gets an instance of the service, suitable for the package of the given class.
    * @param appClass the main application class (only the package name is used as the path to  
    * app-specific preferences storage)
    * @return an instance of the service
    */
   public DefaultPreferencesService(Class appClass)
   {
      prefs = Preferences.userNodeForPackage(appClass);   
   }
   
   public String get(String key, String defval) { return prefs.get(key, defval); }
   public void put(String key, String defval) { prefs.put(key, defval); }
   public boolean isWebStart() { return false; }

   private Preferences prefs;
}