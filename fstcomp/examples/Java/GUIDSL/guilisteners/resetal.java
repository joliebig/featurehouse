import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.*;
//created on: Sun Dec 05 21:40:16 CST 2004
/*
resets the gui to the original state
*/

class resetal implements ActionListener{
    public void actionPerformed(ActionEvent ae){
        grammar.reset();
        ActionList.setGui();
        Gui.tabs.setSelectedIndex(0);//go to the first pane
    }
}