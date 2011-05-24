

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
	private String findword; //for searching & finding the word
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
	/**
	 *If we want to exit from the program, 
	 *first we want to know if the text area is empty or not,
	 *second we want to kwnow if the text was saved or not.
	 *If the text area isn't empty & the text wasn't saved befor this time,
	 *then the program display ->
	 *for the user an option for saving the text in a new file or in the same file
	 */
	public void exiT(){
		/**
		 *if the text area isn't empty, prompt for quiting
		 */
		 System.exit(0);
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
	/**
	 *@see ABOUT.JAVA
	 *it's a Message Dialog to show the information about this program
	 */
	public void abouT(){
		JOptionPane.showMessageDialog(null, new About(),"About Notepad",JOptionPane.PLAIN_MESSAGE);
	}

}
