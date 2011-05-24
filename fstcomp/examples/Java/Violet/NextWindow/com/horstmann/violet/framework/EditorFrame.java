package com.horstmann.violet.framework;

public class EditorFrame {

  EditorFrame(Class appClass) {

      windowMenu.add(factory.createMenuItem(
         "window.next", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
               JInternalFrame[] frames = desktop.getAllFrames();
               for (int i = 0; i < frames.length; i++)
               {
                  if (frames[i] == desktop.getSelectedFrame())
                  {
                     i++; 
                     if (i == frames.length) i = 0;
                     try
                     {
                        frames[i].toFront();
                        frames[i].setSelected(true); 
                     }
                     catch (PropertyVetoException exception)
                     {
                     }
                     return;
                  }
               }
            }
         }));
  }

}