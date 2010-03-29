package com.horstmann.violet.framework;

public class EditorFrame {

  EditorFrame(Class appClass) {

      editMenu.add(factory.createMenuItem(
         "edit.select_next", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
               GraphFrame frame 
                  = (GraphFrame)desktop.getSelectedFrame();
               if (frame == null) return;
               GraphPanel panel = frame.getGraphPanel();
               panel.selectNext(1);
            }
         }));
  }

}