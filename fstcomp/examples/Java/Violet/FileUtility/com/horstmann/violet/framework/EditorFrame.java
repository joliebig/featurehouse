package com.horstmann.violet.framework;

public class EditorFrame {

   protected FileService fileService;
   protected ArrayList recentFiles;

   File lastDir = new File(".");
         
   EditorFrame(Class appClass) {
         fileService = FileService.getInstance(lastDir);      
   }
}