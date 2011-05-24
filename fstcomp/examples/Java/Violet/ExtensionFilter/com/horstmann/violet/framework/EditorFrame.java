package com.horstmann.violet.framework;

public class EditorFrame {
   protected ExtensionFilter violetFilter;
   protected ExtensionFilter exportFilter;
   protected String defaultExtension;

   EditorFrame(Class appClass) {
        defaultExtension = appResources.getString("files.extension");
   }
}