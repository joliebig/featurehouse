package com.horstmann.violet.framework;

public class EditorFrame {

  EditorFrame(Class appClass) {

      windowMenu.add(factory.createMenuItem(
         "window.restore", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
               GraphFrame frame 
                  = (GraphFrame)desktop.getSelectedFrame();
               if (frame == null) return;
               try
               {
                  frame.setMaximum(false);
               }
               catch (PropertyVetoException exception)
               {
               }
            }
         }));
  }

}