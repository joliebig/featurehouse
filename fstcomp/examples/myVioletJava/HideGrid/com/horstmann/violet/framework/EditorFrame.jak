package com.horstmann.violet.framework;

public class EditorFrame {

  EditorFrame(Class appClass) {

      final JCheckBoxMenuItem hideGridItem;
      viewMenu.add(hideGridItem = (JCheckBoxMenuItem) factory.createCheckBoxMenuItem(
         "view.hide_grid", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
               GraphFrame frame 
                  = (GraphFrame)desktop.getSelectedFrame();
               if (frame == null) return;
               GraphPanel panel = frame.getGraphPanel();
               JCheckBoxMenuItem menuItem = (JCheckBoxMenuItem) event.getSource();               
               panel.setHideGrid(menuItem.isSelected());
            }
         }));
         
      viewMenu.addMenuListener(new
            MenuListener()
            {
               public void menuSelected(MenuEvent event)
               {
                  GraphFrame frame 
                     = (GraphFrame) desktop.getSelectedFrame();
                  if (frame == null) return;
                  GraphPanel panel = frame.getGraphPanel();
                  hideGridItem.setSelected(panel.getHideGrid());
               }
               public void menuDeselected(MenuEvent event)
               {
               }
               public void menuCanceled(MenuEvent event)
               {                  
               }
            });         
  }

}