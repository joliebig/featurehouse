package com.horstmann.violet.framework;

public class EditorFrame {

   protected static final int FRAME_GAP = 20;
   protected static final int ESTIMATED_FRAMES = 5;

   /**
      Creates an internal frame on the desktop.
      @param c the component to display in the internal frame
      @param t the title of the internal frame.
   */
   protected void addInternalFrame(final JInternalFrame iframe)
   {  
      iframe.setResizable(true);
      iframe.setClosable(true);
      iframe.setMaximizable(true);
      iframe.setIconifiable(true);
      int frameCount = desktop.getAllFrames().length;      
      desktop.add(iframe);
      // position frame
      int emptySpace 
         = FRAME_GAP * Math.max(ESTIMATED_FRAMES, frameCount);
      int width = Math.max(desktop.getWidth() / 2, 
            desktop.getWidth() - emptySpace);            
      int height = Math.max(desktop.getHeight() / 2, 
         desktop.getHeight() - emptySpace);

      iframe.reshape(frameCount * FRAME_GAP, 
         frameCount * FRAME_GAP, width, height);
      iframe.show();

      iframe.addInternalFrameListener(new
         InternalFrameAdapter()
         {
            public void internalFrameActivated(InternalFrameEvent event)
            {
               setTitle();
            }
            public void internalFrameDeactivated(InternalFrameEvent event)
            {
               setTitle();
            }
         });

      // select the frame--might be vetoed
      try
      {  
         iframe.setSelected(true);
      }
      catch(PropertyVetoException e)
      {
      }
   }

}