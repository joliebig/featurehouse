import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.*;
//created on: Sun Dec 05 21:45:30 CST 2004

/*
saves a configuration in a given file
makes sure the extension is .config
asks for confirmation when attempting to override
*/

    class saveconfigal implements ActionListener{
        private Gui current;

        saveconfigal(Gui g){
            current = g;
        }
        public void actionPerformed(ActionEvent ae){
                current.fc.setFileFilter(current.conf);
                int returnVal = current.fc.showSaveDialog(current);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    File file = current.fc.getSelectedFile();
                    //force a .config extension even if the user did not choose to append it
                    String filename = file.getName();

                    int i = filename.lastIndexOf('.');
                    if (! filename.substring(i+1).toLowerCase().equals("config"))
                        if (!file.renameTo(new File(filename + ".config")))
                            //if couldn't rename, create a new file
                            file = new File(filename + ".config");
                    if (file.exists()){
                        if (JOptionPane.showConfirmDialog(current, "This file already exists. Rename?",
                            "Are you sure?", JOptionPane.YES_NO_OPTION) != JOptionPane.YES_OPTION)
                            return; //user is not sure...
                    }
                    current.writeConfig(file);

                }
            }
    }