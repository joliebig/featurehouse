

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
    private int returnVal; //for JFileChooser
    private int option; //for using it into JOptionPane
    private String fileContent = null; //to get the text from the text area
    private String fileName = null; //for using the name of the file
    private JFileChooser jfc = new JFileChooser("."); //for using a open & save dialog
    private ExampleFileFilter filter = new ExampleFileFilter(); //for filtering the file
    Notepad n; //for using the object in the Notepad.java

    public Actions(Notepad n){
        this.n = n;
    }

    /** to be overwritten by Save 
    private void doSaveQuestions() {
    	    return;
    }
    /**
     *If we want to write a new text, first we want to know if the text area is empty or not,
     *second we want to kwnow if the text was saved or not.
     *If the text area isn't empty & the text wasn't saved befor this time, then the program display ->
     *for the user an option for saving the text in a new file or in the same file
     */
    public void neW(){
    	doSaveQuestions();
        n.getTextArea().setText("");
        n.setTitle("Untitled - JAVA� Notepad");
    }
    /**
     *If we want to open a new text, first we want to know if the text area
     * is empty or not, second we want to kwnow if the text was saved or not.
     *If the text area isn't empty & the text wasn't saved befor this time,
     *then the program display for the user an option for saving the text
     *in a new file or in the same file
     */
    public void opeN(){
    	   doSaveQuestions();
	   open();
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
    	   doSaveQuestions();
           System.exit(0);
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
