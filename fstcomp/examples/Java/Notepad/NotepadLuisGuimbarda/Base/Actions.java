

//import the packages for using the classes in them into this class
import java.io.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.filechooser.*;

/**
 *A PUBLIC CLASS FOR ACTIONS.JAVA
 */
public class Actions{
    Notepad n;

    public Actions(Notepad n){
            this.n = n;
    }

    //for closing the program
    public void exiT(){
        System.exit(0);
    }
    /**
     *@see ABOUT.JAVA
     *it's a Message Dialog to show the information about this program
     */
    public void abouT(){
        JOptionPane.showMessageDialog(null, new About(),"About Notepad",JOptionPane.PLAIN_MESSAGE);
    }
}
