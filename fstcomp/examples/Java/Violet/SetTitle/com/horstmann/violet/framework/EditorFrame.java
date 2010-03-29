package com.horstmann.violet.framework;

public class EditorFrame {

    EditorFrame(Class appClass) {
        setTitle(appResources.getString("app.name"));
    }

   /**
      Sets the frame title.
   */
   protected void setTitle()
   {
      String appName = appResources.getString("app.name");
      GraphFrame frame
         = (GraphFrame)desktop.getSelectedFrame();
      if (frame == null)
         setTitle(appName);
      else
      {
         String fileName = frame.getFileName();
         if (fileName == null)
            setTitle(appName);
         else
            setTitle(appName + " - " + fileName);
      }
   }
}