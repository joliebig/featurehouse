package com.horstmann.violet.framework;

public class EditorFrame {

  EditorFrame(Class appClass) {

      windowMenu.add(factory.createMenuItem(
         "window.maximize", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
               GraphFrame frame 
                  = (GraphFrame)desktop.getSelectedFrame();
               if (frame == null) return;
               try
               {
                  frame.setMaximum(true);
               }
               catch (PropertyVetoException exception)
               {
               }
            }
         }));
  }

}