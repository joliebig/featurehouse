package com.horstmann.violet.framework;

public class EditorFrame {

  EditorFrame(Class appClass) {
  
  	  editProperties = true;
  
      editMenu.add(factory.createMenuItem(
         "edit.properties", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
               final GraphFrame frame 
                  = (GraphFrame)desktop.getSelectedFrame();
               if (frame == null) return;
               GraphPanel panel = frame.getGraphPanel();
               panel.editSelected();
            }
         }));
  }

}