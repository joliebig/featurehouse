package com.horstmann.violet.framework;

public class EditorFrame {

  EditorFrame(Class appClass) {
/*      JMenuItem fileSaveItem = factory.createMenuItem(
            "file.save", this, "save");
*/
        JMenuItem fileSaveItem = factory.createMenuItem(
         "file.save", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
            save();
            }
         });

      fileMenu.add(fileSaveItem);
      if (fileService.isWebStart()) fileSaveItem.setEnabled(false);
  }

   public void save()
   {
      GraphFrame frame
         = (GraphFrame) desktop.getSelectedFrame();
      if (frame == null) return;
      String fileName = frame.getFileName();
      if (fileName == null) { saveAs(); return; }
      try
      {
        saveFile(frame.getGraph(), new FileOutputStream(fileName));
         frame.getGraphPanel().setModified(false);
      }
      catch (Exception exception)
      {
         JOptionPane.showInternalMessageDialog(desktop,
            exception);
      }
   }


}