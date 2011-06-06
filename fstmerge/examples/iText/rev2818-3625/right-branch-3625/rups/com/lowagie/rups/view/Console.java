

package com.lowagie.rups.view;

import java.awt.Color;
import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.PrintStream;
import java.util.Observable;
import java.util.Observer;

import javax.swing.JTextPane;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Document;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;


public class Console implements Observer {

    
    private static Console console = null;
    
    
    PrintStream printStream;
    
    PipedOutputStream poCustom;
    
    PipedInputStream piCustom;
    
    
    PipedOutputStream poOut;
    
    PipedInputStream piOut;

    
    PipedOutputStream poErr;
    
    PipedInputStream piErr;
    
    
    ConsoleStyleContext styleContext = new ConsoleStyleContext();
    
    
    JTextPane textArea = new JTextPane(new DefaultStyledDocument(styleContext));

    
    private Console() throws IOException {
        
        piCustom = new PipedInputStream();
        poCustom = new PipedOutputStream();
        printStream = new PrintStream(poCustom);
        
        
        piOut = new PipedInputStream();
        poOut = new PipedOutputStream(piOut);
        System.setOut(new PrintStream(poOut, true));

        
        piErr = new PipedInputStream();
        poErr = new PipedOutputStream(piErr);
        System.setErr(new PrintStream(poErr, true));

        
        textArea.setEditable(false);

        
        new ReadWriteThread(piCustom, ConsoleStyleContext.CUSTOM).start();
        new ReadWriteThread(piOut, ConsoleStyleContext.SYSTEMOUT).start();
        new ReadWriteThread(piErr, ConsoleStyleContext.SYSTEMERR).start();
    }

    
    public static synchronized Console getInstance() {
        if (console == null) {
            try {
                console = new Console();
            } catch (IOException e) {
                console = null;
            }
        }
        return console;
    }

    
    public void update(Observable observable, Object obj) {
        if (RupsMenuBar.CLOSE.equals(obj)) {
            textArea.setText("");
        }
        if (RupsMenuBar.OPEN.equals(obj)) {
            textArea.setText("");
        }
    }
    
    
    public static void println(String s) {
        PrintStream ps = getInstance().getPrintStream();
        if (ps == null) {
            System.out.println(s);
        }
        else {
            ps.println(s);
            ps.flush();
        }
    }

    
    public PrintStream getPrintStream() {
        return printStream;
    }

    
    public JTextPane getTextArea() {
        return textArea;
    }
    
    
    class ReadWriteThread extends Thread {
        
        PipedInputStream pi;
        
        String type;

        
        ReadWriteThread(PipedInputStream pi, String type) {
            super();
            this.pi = pi;
            this.type = type;
        }

        
        public void run() {
            final byte[] buf = new byte[1024];

            while (true) {
                try {
                    final int len = pi.read(buf);
                    if (len == -1) {
                        break;
                    }
                    Document doc = textArea.getDocument();
                    AttributeSet attset = styleContext.getStyle(type);
                    String snippet = new String(buf, 0, len);
                    doc.insertString(doc.getLength(),
                                     snippet, attset);
                    printStream.print(snippet);
                    textArea.setCaretPosition(textArea.getDocument().
                                              getLength());
                } catch (BadLocationException ex) {
                } catch (IOException e) {
                }
            }
        }
    }    
    
    
    class ConsoleStyleContext extends StyleContext {

        
        private static final long serialVersionUID = 7253870053566811171L;
        
        public static final String CUSTOM = "Custom";
        
        public static final String SYSTEMOUT = "SystemOut";
        
        public static final String SYSTEMERR = "SystemErr";

        
        public ConsoleStyleContext() {
            super();
            Style root = getStyle(DEFAULT_STYLE);
            Style s = addStyle(CUSTOM, root);
            StyleConstants.setForeground(s, Color.BLACK);
            s = addStyle(SYSTEMOUT, root);
            StyleConstants.setForeground(s, Color.GREEN);
            s = addStyle(SYSTEMERR, root);
            StyleConstants.setForeground(s, Color.RED);
        }
    }
}
