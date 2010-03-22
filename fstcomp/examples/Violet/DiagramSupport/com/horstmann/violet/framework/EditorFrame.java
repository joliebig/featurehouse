package com.horstmann.violet.framework;

public class EditorFrame {

    protected ResourceFactory appFactory;

    EditorFrame(Class appClass) {
        appFactory = new ResourceFactory(appResources);
    }

   /**
      Adds a graph type to the File->New menu.
      @param resourceName the name of the menu item resource
      @param graphClass the class object for the graph
   */
   public void addGraphType(String resourceName,
      final Class graphClass)
   {
      newMenu.add(appFactory.createMenuItem(resourceName, new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
               try
               {
                  GraphFrame frame = new GraphFrame(
                        (Graph) graphClass.newInstance());
                  addInternalFrame(frame);
               }
               catch (Exception exception)
               {
                  exception.printStackTrace();
               }
            }
         }));
   }

}