package com.horstmann.violet.framework;

public class EditorFrame {

  EditorFrame(Class appClass) {
/*      fileMenu.add(factory.createMenuItem(
         "file.print", this, "print"));
*/
      fileMenu.add(factory.createMenuItem(
         "file.print", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
            print();
            }
         }));
  }

   /**
      Prints the current graph.
   */
   public void print()
   {
      GraphFrame frame
         = (GraphFrame)desktop.getSelectedFrame();
      if (frame == null) return;

      PrintDialog dialog = new PrintDialog(frame.getGraph());
      dialog.setVisible(true);
   }

}