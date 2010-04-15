

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
    private JTextArea textArea;
    //create the Menu Bar that contains the JMenu "filE, ediT, vieW, formaT, helP"
    private JMenuBar Menubar;
    //Create the menu that contains the items
    private JMenu filE   = new JMenu("File"),
                    ediT   = new JMenu("Edit"),
                    vieW   = new JMenu("View"),
                    formaT = new JMenu("Format"),
                    helP   = new JMenu("Help");
    //Create the Tool Bar that contains the JButton
    private JToolBar toolBar;
    //Create the menu items
    private JMenuItem exiT, abouT;
    //Create Scroll pane (JScrollPane) for the JTextArea
    private JScrollPane scrollpane;

    //for using textArea @Actions.java
    public JTextArea getTextArea(){
        return textArea;
    }

    //Constructor of Notepad
    public Notepad(){
        //set the title for Notepad and set the size for it.
        setTitle("Untitled - JAVA� Notepad");
        setSize(800,600);

        //get the graphical user interface components display area
        Container cp = getContentPane();
        /**
         *adding the text area &
         *adding the scroll pane to the container
         */
        cp.add(textArea = new JTextArea());
        conditionalAddToolbar(cp);
        cp.add(scrollpane = new JScrollPane(textArea));

        //for setting the menu bar
        setJMenuBar(Menubar= new JMenuBar());
        //adding file, edit, view, format, help to the menu bar
        Menubar.add(filE   );
        Menubar.add(ediT   );
        Menubar.add(vieW   );
        Menubar.add(formaT );
        Menubar.add(helP   );

        /**
         *adding exiT to the filE Menu &
         *adding a small image icon to the menu item
         */
        filE.add(exiT   = new JMenuItem("Exit")); //, new ImageIcon(this.getClass().getResource("images/exit.gif"))));  -- exit.gif missing

        /**
         *adding abouT to the helP Menu &
         *adding a samll image icon to the menu item
         */
        helP.add(abouT = new JMenuItem("About Notepad", new ImageIcon(this.getClass().getResource("images/about.gif"))));

        /**
         *allowing the file   menu to be selected by pressing ALT + F
         *allowing the edit   menu to be selected by pressing ALT + E
         *allowing the view   menu to be selected by pressing ALT + V
         *allowing the format menu to be selected by pressing ALT + O
         *allowing the help   menu to be selected by pressing ALT + H
         */
        filE.setMnemonic('f');
        ediT.setMnemonic('e');
        vieW.setMnemonic('v');
        formaT.setMnemonic('o');
        helP.setMnemonic('h');

        /**
         *allowing the exiT      menu item to be selected by pressing ALT + F4
         */
        exiT.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F4, ActionEvent.CTRL_MASK));

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
         *adding action listener for menu item: exiT & abouT
         *the actions was written @Actions.java
         */
        exiT.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                actions.exiT();
            }
        });
        abouT.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                actions.abouT();
            }
        });

        center.nCenter();
        show();
    }
    private void conditionalAddToolbar(Container cp){
    }
    //Main Method
    public static void main(String[] args){
        new Notepad();

    }
}
