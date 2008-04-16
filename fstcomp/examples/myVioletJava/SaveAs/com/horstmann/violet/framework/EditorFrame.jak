package com.horstmann.violet.framework;

import com.horstmann.violet.ArrowHead;
import com.horstmann.violet.BentStyle;
import com.horstmann.violet.LineStyle;

public class EditorFrame {

  EditorFrame(Class appClass) {
      /*fileMenu.add(factory.createMenuItem(
         "file.save_as", this, "saveAs"));*/

      fileMenu.add(factory.createMenuItem(
         "file.save_as", new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
            saveAs();
            }
         }));
  }

   /**
      Saves the current graph as a new file.
   */
   public void saveAs()
   {
      GraphFrame frame
         = (GraphFrame)desktop.getSelectedFrame();
      if (frame == null) return;
      Graph graph = frame.getGraph();
      try
      {
        Save save = fileService.save(null, frame.getFileName(), violetFilter, null, defaultExtension);
        OutputStream out = save.getOutputStream();
        if (out != null)
        {
            try
            {
               saveFile(graph, out);
            }
            finally
            {
               out.close();
            }
            frame.setFileName(save.getName());
            setTitle();
            frame.getGraphPanel().setModified(false);
        }
      }
      catch (IOException exception)
      {
         JOptionPane.showInternalMessageDialog(desktop,
            exception);
      }
   }

   /**
      Saves the current graph in a file. We use long-term
      bean persistence to save the program data. See
      http://java.sun.com/products/jfc/tsc/articles/persistence4/index.html
      for an overview.
      @param out the stream for saving
   */
   protected static void saveFile(Graph graph, OutputStream out)
   {
      XMLEncoder encoder = new XMLEncoder(out);

      encoder.setExceptionListener(new
         ExceptionListener()
         {
            public void exceptionThrown(Exception ex)
            {
               ex.printStackTrace();
            }
         });
      /*
      The following does not work due to bug #4741757

      encoder.setPersistenceDelegate(
         Point2D.Double.class,
         new DefaultPersistenceDelegate(
            new String[]{ "x", "y" }) );
      */
      encoder.setPersistenceDelegate(Point2D.Double.class, new
            DefaultPersistenceDelegate()
            {
               protected void initialize(Class type,
                  Object oldInstance, Object newInstance,
                  Encoder out)
               {
                  super.initialize(type, oldInstance,
                     newInstance, out);
                  Point2D p = (Point2D)oldInstance;
                  out.writeStatement(
                        new Statement(oldInstance,
                           "setLocation", new Object[]{ new Double(p.getX()), new Double(p.getY()) }) );
               }
            });

      encoder.setPersistenceDelegate(BentStyle.class,
         staticFieldDelegate);
      encoder.setPersistenceDelegate(LineStyle.class,
         staticFieldDelegate);
      encoder.setPersistenceDelegate(ArrowHead.class,
         staticFieldDelegate);

      Graph.setPersistenceDelegate(encoder);
      AbstractNode.setPersistenceDelegate(encoder);

      encoder.writeObject(graph);
      encoder.close();
   }

   private static PersistenceDelegate staticFieldDelegate
      = new
         DefaultPersistenceDelegate()
         {
            protected Expression instantiate(Object
               oldInstance, Encoder out)
            {
               try
               {
                  Class cl = oldInstance.getClass();
                  Field[] fields = cl.getFields();
                  for (int i = 0; i < fields.length; i++)
                  {
                     if (Modifier.isStatic(
                            fields[i].getModifiers()) &&
                        fields[i].get(null) == oldInstance)
                     {
                        return new Expression(fields[i],
                           "get",
                           new Object[] { null });
                     }
                  }
               }
               catch (IllegalAccessException ex)
               {
                  ex.printStackTrace();
               }
               return null;
            }

            protected boolean mutatesTo(
               Object oldInstance, Object newInstance)
            {
               return oldInstance == newInstance;
            }
         };


   // workaround for bug #4646747 in J2SE SDK 1.4.0
   private static java.util.HashMap beanInfos;
   static
   {
      beanInfos = new java.util.HashMap();
      Class[] cls = new Class[]
         {
            Point2D.Double.class,
            BentStyle.class,
            ArrowHead.class,
            LineStyle.class,
            Graph.class,
            AbstractNode.class,
         };
      for (int i = 0; i < cls.length; i++)
      {
         try
         {
            beanInfos.put(cls[i],
               java.beans.Introspector.getBeanInfo(cls[i]));
         }
         catch (java.beans.IntrospectionException ex)
         {
         }
      }
   }

}