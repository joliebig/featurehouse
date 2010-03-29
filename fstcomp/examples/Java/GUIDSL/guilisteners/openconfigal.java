import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.*;
//created on: Sun Dec 05 21:55:46 CST 2004
/*
used both for opening .config and .equations files since the algorithm
is pretty much the same
does not verify that the equation is indeed complete at the end
of loading a .equations file
*/

class openconfigal implements ActionListener{
    private Gui current;
    private javax.swing.filechooser.FileFilter ff;
    openconfigal(Gui g, javax.swing.filechooser.FileFilter f){
        current = g;
        ff = f;
    }
    public void actionPerformed(ActionEvent ae){
        current.fc.setFileFilter(ff);
        int returnVal = current.fc.showOpenDialog(current);
        if (returnVal == JFileChooser.APPROVE_OPTION){
            File file = current.fc.getSelectedFile();
            if (! file.exists()){
                JOptionPane.showMessageDialog(current,
                                     "Error",
                                     "File with this name does not exist. Please, try again.",
                                    JOptionPane.INFORMATION_MESSAGE);
                return;
            }
            current.readConfig(file);
        }
    }
}
