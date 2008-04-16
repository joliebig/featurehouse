package com.horstmann.violet.framework;

public class EditorFrame {

   protected JMenu recentFilesMenu;

  EditorFrame(Class appClass) {
        recentFiles = new ArrayList();

        recentFilesMenu = factory.createMenu("file.recent");
        buildRecentFilesMenu();
        fileMenu.add(recentFilesMenu);
  }

     /**
    * Adds a file name to the "recent files" list and rebuilds the "recent files" menu.
    * @param newFile the file name to add
    */
   private void addRecentFile(final String newFile)
   {
      recentFiles.remove(newFile);
      if (newFile == null || newFile.equals("")) return;
      recentFiles.add(0, newFile);
      buildRecentFilesMenu();
   }

   /**
    * Rebuilds the "recent files" menu.
    */
   private void buildRecentFilesMenu()
   {
      recentFilesMenu.removeAll();
      for (int i = 0; i < recentFiles.size(); i++)
      {
         final String file = (String) recentFiles.get(i);
         String name = new File(file).getName();
         if (i < 10) name = i + " " + name;
         else if (i == 10) name = "0 " + name;
         JMenuItem item = new JMenuItem(name);
         if (i < 10) item.setMnemonic((char)('0' + i));
         else if (i == 10) item.setMnemonic('0');
         recentFilesMenu.add(item);
         item.addActionListener(new
               ActionListener()
               {
                  public void actionPerformed(ActionEvent event)
                  {
                     open(file);
                  }
               });
      }
   }

   public void openFile()
   {
        original();
        if (in != null)
        {
            try
            {
                addRecentFile(open.getName());
            }
            catch (IOException exception)
            {
               JOptionPane.showInternalMessageDialog(desktop, exception);
            }
        }
   }

}