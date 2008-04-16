package com.horstmann.violet.framework;

public class EditorFrame {

  EditorFrame(Class appClass) {

      viewMenu.add(factory.createMenuItem(
            "view.clip_drawing_area", new
            ActionListener()
            {
               public void actionPerformed(ActionEvent event)
               {
                  GraphFrame frame 
                     = (GraphFrame) desktop.getSelectedFrame();
                  if (frame == null) return;
                  Graph g = frame.getGraph();
                  Rectangle2D bounds = g.getBounds((Graphics2D) frame.getGraphics());
                  g.setMinBounds(null); 
                  frame.getGraphPanel().revalidate();
                  frame.repaint();
               }
            }));
  }

}