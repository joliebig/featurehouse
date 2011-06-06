


package com.lowagie.toolbox.swing;

import java.awt.Toolkit;

import javax.swing.*;
import javax.swing.text.*;


public class CustomDialog {
    JDialog dialog = null;
    private JTextField textField = new JTextField(10);

    JPanel jPanel1 = new JPanel();
    PlainDocument plainDocument;

    String msgString1;

    Object[] array;

    private JOptionPane optionPane;

    public CustomDialog(String msgstring, PlainDocument plainDocument) {
        super();
        this.setMsgString1(msgstring);
        this.plainDocument = plainDocument;
        try {
            jbInit();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    public CustomDialog() {
        this("Enter a value:", new PlainDocument());
    }

    private void jbInit() throws Exception {
        textField.setDocument(plainDocument);
    }

    public static PlainDocument instantiateFloatDocument() {
        PlainDocument floatDocument = new PlainDocument() {
            private static final long serialVersionUID = 1874451914306029381L;

            public void insertString(int offset, String str, AttributeSet a) throws
                    BadLocationException {
                super.insertString(offset, str, a);
                try {
                    Float.parseFloat(super.getText(0, this.getLength()));
                } catch (Exception ex) {
                    super.remove(offset, 1);
                    Toolkit.getDefaultToolkit().beep();
                    return;
                }
            }
        };
        return floatDocument;
    }

    public static PlainDocument instantiateIntegerDocument() {
        PlainDocument intDocument = new PlainDocument() {
            private static final long serialVersionUID = -8735280090112457273L;

            public void insertString(int offset, String str, AttributeSet a) throws
                    BadLocationException {
                super.insertString(offset, str, a);
                try {
                    Integer.parseInt(super.getText(0, this.getLength()));
                } catch (Exception ex) {
                    super.remove(offset, 1);
                    Toolkit.getDefaultToolkit().beep();
                    return;
                }
            }
        };
        return intDocument;
    }

    public static PlainDocument instantiateStringDocument() {
        PlainDocument stringDocument = new PlainDocument() {
            private static final long serialVersionUID = -1244429733606195330L;

            public void insertString(int offset, String str, AttributeSet a) throws
                    BadLocationException {
                super.insertString(offset, str, a);
            }
        };
        return stringDocument;
    }


    public void setMsgString1(String msgString1) {
        this.msgString1 = msgString1;
        array = new Object[] {msgString1, textField};
        optionPane = new JOptionPane(array, JOptionPane.QUESTION_MESSAGE,
                                     JOptionPane.OK_CANCEL_OPTION);
        dialog = optionPane.createDialog(UIManager.getString(
                "OptionPane.inputDialogTitle", null));
    }

    public String showInputDialog(String startvalue) {
        textField.setText(startvalue);
        dialog.setVisible(true);
        dialog.dispose();
        return textField.getText();
    }
}
