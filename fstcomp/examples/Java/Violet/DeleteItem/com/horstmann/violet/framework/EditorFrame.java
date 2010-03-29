package com.horstmann.violet.framework;

public class EditorFrame {

  EditorFrame(Class appClass) {

      editMenu.add(factory.createMenuItem("edit.delete", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
               GraphFrame frame 
                  = (GraphFrame)desktop.getSelectedFrame();
               if (frame == null) return;
               GraphPanel panel = frame.getGraphPanel();
               panel.removeSelected();
            }
         }));
  }

}