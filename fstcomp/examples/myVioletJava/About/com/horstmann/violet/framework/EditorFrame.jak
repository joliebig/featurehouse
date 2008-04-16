package com.horstmann.violet.framework;

public class EditorFrame {

  private ResourceBundle versionResources;

  EditorFrame(Class appClass) {

      versionResources = ResourceBundle.getBundle(appClassName + "Version");
      
/*      helpMenu.add(factory.createMenuItem(
         "help.about", this, "showAboutDialog"));
*/

      helpMenu.add(factory.createMenuItem(
         "help.about", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
            showAboutDialog();
            }
         }));         
  }

   /**
      Displays the About dialog box.
   */
   public void showAboutDialog()
   {
      MessageFormat formatter = new MessageFormat(
            editorResources.getString("dialog.about.version"));
      JOptionPane.showInternalMessageDialog(desktop, 
         formatter.format(new Object[] { 
               appResources.getString("app.name"),
               versionResources.getString("version.number"),
               versionResources.getString("version.date"),
               appResources.getString("app.copyright"),
               editorResources.getString("dialog.about.license")}),
         null, 
         JOptionPane.INFORMATION_MESSAGE,
         new ImageIcon(
            getClass().getResource(appResources.getString("app.icon"))));  
   }

}