

/**
 *This is a JAVA� program for writing, opening, saving, editing the documents,
 *has the ability to copy, cut, paste & select all text in the JTextArea,
 *has @see ExampleFileFilter class (From SUN� -http://java.sun.com-) for filter the file
 *has a print class (from AarbTeam2000� -http://wwww.arabteam2000.com-) for printing the documents
 */

/**
 *@King Fahd University of Petroleum and Minerals (KFUPM)
 *@auther: Al-Thubaiti Salah
 *@ICS201 PROJECT
 *@JAVA� Notepad (JNotepad)
 *@version# 2.0
 */
 
//import the packages for using the classes in them into the program
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.undo.*;

/**
 *A PUBLIC CLASS FOR NOTEPAD.JAVA
 */
public class Notepad extends JFrame{
	//for using the methods in these classes
	public Actions actions = new Actions(this);
	public Center center = new Center(this);
	
	//declaration of the private variables used in the program
	//create the text area
    JTextArea textArea;
	//create the Menu Bar that contains the JMenu "filE, ediT, vieW, formaT, helP"
    JMenuBar Menubar;
	//Create the menu that contains the items
     JMenu filE, ediT, helP;
	//Create the menu items
    private JMenuItem exiT, abouT, finD, findNexT;
	//Create the Tool Bar that contains the JButton
	 JToolBar toolBar;
	//Create the buttons
	private JButton findButton, aboutButton;
	//Create Scroll pane (JScrollPane) for the JTextArea
	private JScrollPane scrollpane;

    public JTextArea getTextArea(){
	return textArea;
    }

    void MenuFileIOHook(){
    }   	 

    void MenuPrintHook(){
    }   	 

    void MenuFormatHook(){
    }   	 

    void MenuCutPasteHook(){
    }   	 

    void MenuRedoHook(){
    }   	 

    void MenuSelectAllHook(){
    }

    void ToolBarPrintHook(){
    }   	 

    void ToolBarFileIOHook(){
    }   	 

    void ToolBarCutPasteHook(){
    }   	 

    void ToolBarRedoHook(){
    }   	 

    void ToolBarFormatHook(){
    }   	 


	//Constructor of Notepad
	public Notepad(){
		//set the title for Notepad and set the size for it.
		setTitle("Untitled - JAVA� Notepad");
		setSize(800,600);
		
		//get the graphical user interface components display area
		Container cp = getContentPane();
		/**
		 *adding the text area,
		 *adding the tool bar &
		 *adding the scroll pane to the container
		 */
		cp.add(textArea = new JTextArea());
		cp.add("North", toolBar = new JToolBar("Tool Bar"));
		cp.add(scrollpane = new JScrollPane(textArea)); 

		//for setting the menu bar
		setJMenuBar(Menubar= new JMenuBar());
		//adding file, edit, view, format, help to the menu bar
		Menubar.add(filE   = new JMenu("File"));
		Menubar.add(ediT   = new JMenu("Edit"));
		MenuFormatHook();
		Menubar.add(helP   = new JMenu("Help"));

		MenuFileIOHook();
		MenuPrintHook();
		filE.add(exiT   = new JMenuItem("Exit"));

 		/**
		 *adding finD, findNexT to the ediT Menu,
		 *adding a small image icon to the menu item &
		 *adding separator between the menu item
		 */
		 MenuRedoHook();
		 MenuCutPasteHook();
		ediT.add(finD = new JMenuItem("Find", new ImageIcon(this.getClass().getResource("images/find.gif"))));
		ediT.add(findNexT = new JMenuItem("Find Next"));
		MenuSelectAllHook();
		/**
		 *adding abouT to the helP Menu & 
		 *adding a samll image icon to the menu item
		 */
		helP.add(abouT = new JMenuItem("About Notepad", new ImageIcon(this.getClass().getResource("images/about.gif"))));
		
		/**
		 *allowing the file   menu to be selected by pressing ALT + F
		 *allowing the edit   menu to be selected by pressing ALT + E
		 *allowing the help   menu to be selected by pressing ALT + H
		 */
		filE.setMnemonic('f');
		ediT.setMnemonic('e');
		helP.setMnemonic('h');
		
		/**
		 *allowing the exiT      menu item to be selected by pressing ALT + F4
		 *allowing the finD      menu item to be selected by pressing ALT + F
		 *allowing the findNexT  menu item to be selected by pressing ALT + F3
		 */
		finD.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, ActionEvent.CTRL_MASK));
		findNexT.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F3, ActionEvent.CTRL_MASK));
		exiT.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F4, ActionEvent.CTRL_MASK));
		
		/**
		 *adding findButton & aboutButton to the tool bar,
		 *adding a small image icon to the menu item &
		 *adding separator between the button
		 */
		 ToolBarFileIOHook();
		 ToolBarPrintHook();
		 ToolBarRedoHook();
		 ToolBarCutPasteHook();
		toolBar.add(findButton  = new JButton(new ImageIcon(this.getClass().getResource("images/find.gif"))));
		findButton.setToolTipText("Find");
		toolBar.addSeparator();
		ToolBarFormatHook();
		toolBar.add(aboutButton = new JButton(new ImageIcon(this.getClass().getResource("images/about.gif"))));
		
		//adding a tool tip text to the button for descriping the image icon.

		aboutButton.setToolTipText("About Notepad");
		
		/**
		 *setting the default close operation to false &
		 *using own action (exiT action @Actions.java)
		 */
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowAdapter(){
			public void windowClosing(WindowEvent e){
				actions.exiT();
			}
		});
		/**
		 *adding action listener for menu item: finD, findNexT, exiT & abouT
		 *the actions was written @Actions.java
		 */
		exiT.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.exiT();
			}
		});
		finD.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.finD();
			}
		});
		findNexT.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.findNexT();
			}
		});
		abouT.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
			    actions.abouT();
			}
		    });
		exiT.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
			    actions.exiT();
			}
		    });
		/**
		 *adding action listener for the button in the tool bar: newButton, openButton,
		 *saveButton, saveAsButton, printButton, redoButton, undoButton, copyButton,
		 *cutButton, pasteButton, findButton, selectALL, lineWraP, fontButton & aboutButton
		 *the actions was written @Actions.java
		 */
		findButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.finD();
			}
		});
		aboutButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.abouT();
			}
		});			
		/**
		 *Setting the Line Wrap & Wrap Style Word features are true 
		 */
		textArea.setLineWrap(true);
		textArea.setWrapStyleWord(true);
		/**
		 *for making the program at the center, 
		 *@see Center.java 
		 */
		center.nCenter();
		show();
	}
	//Main Method
	public static void main(String[] args){
		new Notepad();
		
	}
}			
