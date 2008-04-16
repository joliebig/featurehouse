import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.*;
//created on: Sun Dec 05 21:58:10 CST 2004

/*
brings up a help file (url either local to the computer or taken from the internet)
in a separate window
*/

class helpfileal implements ActionListener{
    private Gui current;
    helpfileal(Gui g){
        current = g;
    }
    public void actionPerformed(ActionEvent ae){
        if (grammar.rootProduction.var.helpfile == null){
            JOptionPane.showMessageDialog(current,
                                     "No help file was provided when generating the GUI",
                                     "Cannot display help",
                                    JOptionPane.INFORMATION_MESSAGE);
            return;
        }
        try{
            JEditorPane editorPane = new JEditorPane();
            editorPane.setEditable(false);
            java.net.URL helpURL = new java.net.URL(grammar.rootProduction.var.helpfile);
            if (helpURL != null) {
                try {
                    editorPane.setPage(helpURL);
                }
                catch (IOException e) {
                    System.err.println("Attempted to read a bad URL: " + helpURL);
                    return;
                }
            }
            else {
                System.err.println("Couldn't find file: " + grammar.rootProduction.var.helpfile);
                return;
            }

                    // Create and set up the window
                    JFrame frame = new JFrame("Help File");
                    frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

                    // Create and set up the content pane
                    JPanel contentPane = new JPanel();
                    contentPane.setLayout(new BorderLayout());
                    JScrollPane editorScrollPane = new JScrollPane(editorPane);
                    editorScrollPane.setVerticalScrollBarPolicy(
                        JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
                    editorScrollPane.setPreferredSize(new Dimension(500, 300));
                    editorScrollPane.setMinimumSize(new Dimension(10, 10));
                    contentPane.add(editorScrollPane);
                    contentPane.setOpaque(true);        // content panes must be opaque
                    frame.setContentPane(contentPane);

                    // Display the window
                    frame.pack();
                    frame.setVisible(true);
                }
                catch (java.net.MalformedURLException e){
                    JOptionPane.showMessageDialog(current,
                                     "I/O exception occured while opening url given: " + e.getMessage(),
                                     "Error",
                                    JOptionPane.INFORMATION_MESSAGE);
                    return;
                }




                }
      }