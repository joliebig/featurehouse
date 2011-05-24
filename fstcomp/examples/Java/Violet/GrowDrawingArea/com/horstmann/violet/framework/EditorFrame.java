package com.horstmann.violet.framework;

public class EditorFrame {

  private static final double GROW_SCALE_FACTOR = Math.sqrt(2);
  
  EditorFrame(Class appClass) {

      viewMenu.add(factory.createMenuItem(
            "view.grow_drawing_area", new
            ActionListener()
            {
               public void actionPerformed(ActionEvent event)
               {
                  GraphFrame frame 
                     = (GraphFrame) desktop.getSelectedFrame();
                  if (frame == null) return;
                  Graph g = frame.getGraph();
                  Rectangle2D bounds = g.getBounds((Graphics2D) frame.getGraphics());
                  bounds.add(frame.getGraphPanel().getBounds());
                  g.setMinBounds(new Rectangle2D.Double(0, 0, 
                        GROW_SCALE_FACTOR * bounds.getWidth(), 
                        GROW_SCALE_FACTOR * bounds.getHeight()));
                  frame.getGraphPanel().revalidate();
                  frame.repaint();
               }
            }));
  }

}