package com.horstmann.violet.framework;

import com.horstmann.violet.UMLEditor;

public class EditorFrame {

  EditorFrame(Class appClass) {

      JMenu lafMenu = factory.createMenu("view.change_laf");
      viewMenu.add(lafMenu);
      
      UIManager.LookAndFeelInfo[] infos =
         UIManager.getInstalledLookAndFeels();
      for (int i = 0; i < infos.length; i++)
      {
         final UIManager.LookAndFeelInfo info = infos[i];
         JMenuItem item = new JMenuItem(info.getName());
         lafMenu.add(item);
         item.addActionListener(new
            ActionListener()
            {
               public void actionPerformed(ActionEvent event)
               {
                  String laf = info.getClassName();
                  changeLookAndFeel(laf);
                  SaveLookAndFeelPreferences(laf);
               }
            });
      }
  }
  
    /**
    * Changes the look and feel
    * @param lafName the name of the new look and feel
    */   
   public void changeLookAndFeel(String lafName)
   {
      try
      {
         UIManager.setLookAndFeel(lafName);
         SwingUtilities.updateComponentTreeUI(UMLEditor.frame);
      }
      catch (ClassNotFoundException ex) {}
      catch (InstantiationException ex) {}
      catch (IllegalAccessException ex) {}
      catch (UnsupportedLookAndFeelException ex) {}
   }

   void SaveLookAndFeelPreferences(String laf)
   {
   		original(laf);
   }

}