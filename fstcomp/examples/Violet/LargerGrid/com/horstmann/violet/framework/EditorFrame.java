package com.horstmann.violet.framework;

public class EditorFrame {

  EditorFrame(Class appClass) {

      viewMenu.add(factory.createMenuItem(
         "view.larger_grid", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
               GraphFrame frame 
                  = (GraphFrame)desktop.getSelectedFrame();
               if (frame == null) return;
               GraphPanel panel = frame.getGraphPanel();
               panel.changeGridSize(1);
            }
         }));
  }

}