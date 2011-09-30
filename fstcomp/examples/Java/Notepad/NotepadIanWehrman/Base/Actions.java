

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
    //declaration of the private variables used in the program
    int returnVal; //for JFileChooser
    int option; //for using it into JOptionPane 
    String fileContent = null; //to get the text from the text area
    String fileName = null; //for using the name of the file
    String findword; //for searching & finding the word
    JFileChooser jfc = new JFileChooser("."); //for using a open & save dialog
    ExampleFileFilter filter = new ExampleFileFilter(); //for filtering the file
    Notepad n; //for using the object in the Notepad.java

    public Actions(Notepad n){
        this.n = n;
    }
    /**
     *If we want to write a new text, first we want to know if the text area is empty or not,
     *second we want to kwnow if the text was saved or not.
     *If the text area isn't empty & the text wasn't saved befor this time, then the program display ->
     *for the user an option for saving the text in a new file or in the same file
     */
    public void neW(){
        n.getTextArea().setText("");
        n.setTitle("Untitled - JAVA Notepad");
    }
    /**
     *If we want to open a new text, first we want to know if the text area
     * is empty or not, second we want to kwnow if the text was saved or not.
     *If the text area isn't empty & the text wasn't saved befor this time,
     *then the program display for the user an option for saving the text 
     *in a new file or in the same file
     */
    public void opeN(){
        open();
    }

    /**
     *THIS FROM SUN� WEBSITE (@Print.java)
     *if we want to print the text, we can do this by print method
     */
    public void prinT(){
        //import printer class
        Print.printComponent(n.getTextArea());
    }
    /**
     *If we want to exit from the program, 
     *first we want to know if the text area is empty or not,
     *second we want to kwnow if the text was saved or not.
     *If the text area isn't empty & the text wasn't saved befor this time,
     *then the program display ->
     *for the user an option for saving the text in a new file or in the same file
     */
    public void exiT(){
        //for closing the program
        System.exit(0);
        
    }
    //to cut the selected text
    public void cuT(){
        n.getTextArea().cut();
    }
    //to copy the selected text
    public void copY(){
        n.getTextArea().copy();
    }
    //to paste the selected text
    public void pastE(){
        n.getTextArea().paste();
    }
    //to select all the text
    public void selectALL(){
        n.getTextArea().selectAll();
    }
    //this is a method for searching the input text from the text area
    public void finD(){
        try{
            //this is an input dialog which return a string (findword)
            findword = JOptionPane.showInputDialog("Type the word to find");
            //if the JTextField in the input dialog is empty (null), then return a message dialog
            while(n.getTextArea().getText().indexOf(findword) == -1){
                /**
                 *this is a message dialog which is warning the user,
                 *because he didn't or forgot to enter the word
                 */
                JOptionPane.showMessageDialog(null,"Word not found!","No match",JOptionPane.WARNING_MESSAGE);
                findword = JOptionPane.showInputDialog("Type the word to find");
            }
            //for selecting the word which the user search for it
            n.getTextArea().select(n.getTextArea().getText().indexOf(findword),
                                   n.getTextArea().getText().indexOf(findword) + findword.length());
        }
        catch(Exception ex){
            JOptionPane.showMessageDialog(null,"Search canceled","Abourted",JOptionPane.WARNING_MESSAGE);
        }
    }
    public void findNexT(){
        n.getTextArea().select(n.getTextArea().getText().indexOf(findword,(int)n.getTextArea().getText().indexOf(findword)+1),
                               n.getTextArea().getText().indexOf(findword,(int)n.getTextArea().getText().indexOf(findword)+1));
    }
    //for wraping the line & wraping the style word
    public void lineWraP(){
        if(n.getLineWrap().isSelected()){
            /**
             *make the line wrap & wrap style word is true
             *when the line wrap is selected
             */
            n.getTextArea().setLineWrap(true);
            n.getTextArea().setWrapStyleWord(true);
        }
        else{
            /**
             *make the line wrap & wrap style word is false
             *when the line wrap isn't selected
             */
            n.getTextArea().setLineWrap(false);
            n.getTextArea().setWrapStyleWord(false);
        }
    }

    /**
     *@see ABOUT.JAVA
     *it's a Message Dialog to show the information about this program
     */
    public void abouT(){
        JOptionPane.showMessageDialog(null, new About(),"About Notepad",JOptionPane.PLAIN_MESSAGE);
    }
    /**
     *THIS IS THE WAY FOR OPENING THE TEXT FILE
     */
    public void open(){
        //filter the kind of files, we want only TXT file
        filter.addExtension("txt");
        //to set a description for the file (TXT)
        filter.setDescription("TXT Documents");
        //setting the FileFilter to JFileChooser
        jfc.setFileFilter(filter);
        returnVal = jfc.showOpenDialog(n); //to show JFileChooser
        if(returnVal == JFileChooser.APPROVE_OPTION){
            //to erase any text in the text area before adding new text
            n.getTextArea().setText(null);
            try{
                //to get the name of the selected file
                fileName = jfc.getSelectedFile().getPath();
                //to read the selected file 
                Reader in = new FileReader(jfc.getSelectedFile());
                //100000 is the max. char can be written in the text area
                char[] buff = new char[100000];
                int nch;
                while((nch = in.read(buff, 0, buff.length)) != -1)
                    n.getTextArea().append(new String(buff, 0, nch));
                //to get more text from the file if the array wasn't full
                fileContent = n.getTextArea().getText();
            }
            catch(FileNotFoundException x){}
            catch(IOException ioe){
                System.err.println("I/O Error on Open");
            }
        }
        n.setTitle(jfc.getSelectedFile().getName() + " - JAVA� Notepad");
    }

}
