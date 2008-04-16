// this extension looks to see if the model file of guidsl is
// in a model directory (test: pathname is model/model.m)
// if so, we examine all subdirectories (layers) of the model
// directory.  Any TOKEN that has a matching corresponding layer
// name "Token".toUpperCase(), this is the name that is output
// when generating an equations file.  Further if there is a
// layer/help.html file, then this is assigned to the helphtml
// attribute of the corresponding variable.

import java.io.*;
import javax.swing.*;
import Jakarta.util.*;
import java.util.*;

class Main  {

   static char buffer[] = new char[1000];

   static public void process( Model m ) throws SemanticException {
       original(m);

       if (modelMode) {
          try { harvestInfo(); }
          catch (IOException e) {
            JOptionPane.showMessageDialog(null,
             "Model Harvesting Error -- see command line for details",
             "Error!", JOptionPane.ERROR_MESSAGE);
            System.err.println(e.getMessage());
          }
       }
    }

    File root = null;
    File par = null;
    final static String modelExtension = ".m";
    final static String helpfile = "help.html";
    final static String featureExpl = "feature.expl";

    static void harvestInfo() throws IOException {
      // Step 1: get par directory

      File root = new File(".");
      File par = new File( root.getAbsolutePath() ).getParentFile();

      // Step 2: create file filter that retrieves
      //         only the subdirectories in this directory

      FileFilter ff = new FileFilter() {
         public boolean accept(File pathname) {
            return pathname.isDirectory();
          }
      };

      // Step 3: read all the subdirectories (layers) in this model

      File[] list = par.listFiles(ff);
      for (int i=0; i<list.length; i++) {
         String layerName = list[i].getName();
         variable v = variable.find( layerName );

         if (v != null) {
            if (v.type != variable.Prim && Main.debug)
               Util.error("feature " + v.name + " is not a primitive in this directory");

            String hname = layerName + File.separator + helpfile;
            File html = new File( hname );
            if (html.isFile())
               v.helpfile = hname;
            File help = new File( layerName + File.separator + featureExpl );
            if (help.isFile()) {
               // this is a hack -- feature files are at most 1000 chars long
               FileReader fr = new FileReader( help );
               fr.read( buffer );
               v.help = new String(buffer);
            }
         }
      }

      // Step 4: make sure that all terminals in the grammar are actually layers

      if (!Main.debug) return;
      Iterator i = variable.Vtable.values().iterator();
      while (i.hasNext()) {
          variable v = (variable) i.next();
          if (v.type != variable.Prim) continue;
          File testname = new File( v.name );
          if (!testname.isDirectory())
             Util.error( v.name + " is a grammar primitive but not implemented ");
      }
   }
}
