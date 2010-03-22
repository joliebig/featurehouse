package com.horstmann.violet.framework;

public class EditorFrame {

  EditorFrame(Class appClass) {

      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      addWindowListener(new
         WindowAdapter()
         {
            public void windowClosing(WindowEvent event)
            {
               exit();
            }
         });

/*      fileMenu.add(factory.createMenuItem(
         "file.exit", this, "exit"));
*/
      fileMenu.add(factory.createMenuItem(
         "file.exit", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
            exit();
            }
         }));
  }

   /**
      Exits the program if no graphs have been modified
      or if the user agrees to abandon modified graphs.
   */
   public void exit()
   {
      int modcount = 0;
      JInternalFrame[] frames = desktop.getAllFrames();
      for (int i = 0; i < frames.length; i++)
      {
         if (frames[i] instanceof GraphFrame)
         {
            GraphFrame frame = (GraphFrame)frames[i];
            if (frame.getGraphPanel().isModified()) modcount++;
         }
      }
      if (modcount > 0)
      {
         // ask user if it is ok to close
         int result
            = JOptionPane.showInternalConfirmDialog(
               desktop,
               MessageFormat.format(editorResources.getString("dialog.exit.ok"),
                     new Object[] { new Integer(modcount) }),
               null,
               JOptionPane.YES_NO_OPTION);

         // if the user doesn't agree, veto the close
         if (result != JOptionPane.YES_OPTION)
            return;
      }
      savePreferences();
      System.exit(0);
   }

   public void savePreferences()
   {
   		original();
   }
   
}