package com.horstmann.violet.framework;

public class EditorFrame {

   /**
      Reads a graph file
      @param in the input stream to read
      @return the graph that is read in
   */
   public static Graph read(InputStream in)
      throws IOException
   {
      XMLDecoder reader 
         = new XMLDecoder(in);
      Graph graph = (Graph) reader.readObject();
      in.close();
      return graph;
   }

}