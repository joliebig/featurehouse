package com.horstmann.violet.framework;

public class EditorFrame {

  protected String imageExtensions;
         
  EditorFrame(Class appClass) {
/*      fileMenu.add(factory.createMenuItem(
         "file.export_image", this, "exportImage"));
*/
      fileMenu.add(factory.createMenuItem(
         "file.export_image", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
            exportImage();
            }
         }));
  }

   /**
      Exports the current graph to an image file.
   */
   public void exportImage()
   {
      GraphFrame frame
         = (GraphFrame)desktop.getSelectedFrame();
      if (frame == null) return;

      try
      {
        Save save = fileService.save(null, frame.getFileName(), exportFilter,
               defaultExtension, imageExtensions);
        OutputStream out = save.getOutputStream();
        if (out != null)
        {
            String format;
            String fileName = save.getName();
            if (fileName == null)
            {
               int n = imageExtensions.indexOf("|");
               if (n < 0) n = imageExtensions.length();
               format = imageExtensions.substring(1, n);
            }
            else
                format = fileName.substring(fileName.lastIndexOf(".") + 1);
            if (!ImageIO.getImageWritersByFormatName(format)
               .hasNext())
            {
               MessageFormat formatter = new MessageFormat(
                  editorResources.getString("error.unsupported_image"));
               JOptionPane.showInternalMessageDialog(desktop,
                  formatter.format(new Object[] { format }));
               return;
            }

            Graph graph = frame.getGraph();
            try
            {
               saveImage(graph, out, format);
            }
            finally
            {
               out.close();
            }
         }
      }
      catch (Exception exception)
      {
         JOptionPane.showInternalMessageDialog(desktop,
            exception);
      }
   }

   /**
      Exports a current graph to an image file.
      @param graph the graph
      @param out the output stream
      @param format the image file format
   */
   public static void saveImage(Graph graph, OutputStream out, String format)
      throws IOException
   {
      BufferedImage dummy = new BufferedImage(1, 1,
         BufferedImage.TYPE_INT_RGB);
      // need a dummy image to get a Graphics to
      // measure the size
      Rectangle2D bounds = graph.getBounds(
         (Graphics2D) dummy.getGraphics());
      BufferedImage image
         = new BufferedImage((int)bounds.getWidth() + 1,
            (int)bounds.getHeight() + 1,
            BufferedImage.TYPE_INT_RGB);
      Graphics2D g2 = (Graphics2D)image.getGraphics();
      g2.translate(-bounds.getX(), -bounds.getY());
      g2.setColor(Color.WHITE);
      g2.fill(new Rectangle2D.Double(
                 bounds.getX(),
                 bounds.getY(),
                 bounds.getWidth() + 1,
                 bounds.getHeight() + 1));
      g2.setColor(Color.BLACK);
      g2.setBackground(Color.WHITE);
      graph.draw(g2, null);
      ImageIO.write(image, format, out);
   }

}