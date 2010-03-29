package com.horstmann.violet.framework;

import java.beans.EventHandler;

public class EditorFrame {

  protected Open open;
  protected InputStream in;

  EditorFrame(Class appClass) {
   /*   fileMenu.add(factory.createMenuItem(
         "file.open", this, "openFile"));
   */
      fileMenu.add(factory.createMenuItem(
         "file.open", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
            openFile();
            }
         }));
  }

   /**
      Asks the user to open a graph file.
   */
   public void openFile()
   {
      try
      {
         open = fileService.open(null, null, violetFilter);
         in = open.getInputStream();
         if (in != null)
         {
            Graph graph = read(in);
            GraphFrame frame = new GraphFrame(graph);
            addInternalFrame(frame);
            frame.setFileName(open.getName());
//            addRecentFile(open.getName());
            setTitle();
         }
      }
      catch (IOException exception)      {
         JOptionPane.showInternalMessageDialog(desktop,
            exception);
      }
   }


   /**
    * Opens a file with the given name, or switches to the frame if it is already open.
    * @param name the file name
    */
   public void open(String name)
   {
      JInternalFrame[] frames = desktop.getAllFrames();
      for (int i = 0; i < frames.length; i++)
      {
         if (frames[i] instanceof GraphFrame)
         {
            GraphFrame frame = (GraphFrame)frames[i];
            if (frame.getFileName().equals(name))
            {
               try
               {
                  frame.toFront();
                  frame.setSelected(true);
               }
               catch (PropertyVetoException exception)
               {
               }
               return;
            }
         }
      }

      try
      {
         Graph graph = read(new FileInputStream(name));
         GraphFrame frame = new GraphFrame(graph);
         addInternalFrame(frame);
         frame.setFileName(name);
      }
      catch (IOException exception)
      {
         JOptionPane.showInternalMessageDialog(desktop,
               exception);
      }
   }

}