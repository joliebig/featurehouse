//created on: Sun Mar 06 15:35:29 CST 2005
import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.*;
//created on: Sun Dec 05 21:45:30 CST 2004

/*
saves an equation in a given file
makes sure the extension is .equat
asks for confirmation when attempting to override
*/

class saveequat implements ActionListener{
        private Gui current;

        saveequat(Gui g){
            current = g;
        }
        public void actionPerformed(ActionEvent ae){
                //check if predicates are satisfied, otherwise complain
               if (!cnfClause.complete(true)){
                   return;
               }
                current.fc.setFileFilter(current.equat);
                int returnVal = current.fc.showSaveDialog(current);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    File file = current.fc.getSelectedFile();
                    String filename = file.getName();
                    int i = filename.lastIndexOf('.');
                    if (! filename.substring(i + 1).toLowerCase().equals(Gui.equations))
                        if (!file.renameTo(new File(filename + "." + Gui.equations)))
                            //if couldn't rename, create a new file
                            file = new File(filename + "."+ Gui.equations);
                    if (file.exists()){
                        if (JOptionPane.showConfirmDialog(current, "This file already exists. Rename?",
                            "Are you sure?", JOptionPane.YES_NO_OPTION) != JOptionPane.YES_OPTION)
                            return; //user is not sure...
                    }
                    current.writeConfig(file);
                    outeqns o = new outeqns( file);
                    grammar.current.visit( o );
                }
    }
}
