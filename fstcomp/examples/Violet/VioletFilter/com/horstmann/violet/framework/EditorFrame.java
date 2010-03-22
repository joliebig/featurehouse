package com.horstmann.violet.framework;

public class EditorFrame {

    EditorFrame(Class appClass) {
      violetFilter = new ExtensionFilter(
         appResources.getString("files.name"), 
         new String[] { defaultExtension });
         
      defaultExtension = appResources.getString("files.extension");
    }
}