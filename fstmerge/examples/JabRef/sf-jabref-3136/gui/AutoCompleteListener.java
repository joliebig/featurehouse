package net.sf.jabref.gui;

import javax.swing.text.JTextComponent;
import javax.swing.text.BadLocationException;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.FocusListener;
import java.awt.event.FocusEvent;


public class AutoCompleteListener extends KeyAdapter implements FocusListener {


    AutoCompleter completer;
    protected String toSetIn = null,
            lastBeginning = null;
    protected int lastCaretPosition = -1;
    protected Object[] lastCompletions = null;
    protected int lastShownCompletion = 0;

    
    
    
    protected FocusListener nextFocusListener = null;

    

    public AutoCompleteListener(AutoCompleter completer) {
        this.completer = completer;
    }

    
    public void setNextFocusListener(FocusListener listener) {
        this.nextFocusListener = listener;
    }

    public void keyPressed(KeyEvent e) {
        if ((toSetIn != null) && (e.getKeyCode() == KeyEvent.VK_ENTER)) {
            JTextComponent comp = (JTextComponent) e.getSource();
            int end = comp.getSelectionEnd();
            comp.select(end, end);
            e.consume();
            return;
        }
        
        else if ((e.getKeyCode() == KeyEvent.VK_PAGE_DOWN) && (lastCompletions != null)) {
            cycle((JTextComponent) e.getSource(), 1);
            e.consume();
        }
        else if ((e.getKeyCode() == KeyEvent.VK_PAGE_UP) && (lastCompletions != null)) {
            cycle((JTextComponent) e.getSource(), -1);
            e.consume();
        }
    }

    private void cycle(JTextComponent comp, int increment) {
        lastShownCompletion += increment;
        if (lastShownCompletion >= lastCompletions.length)
            lastShownCompletion = 0;
        else if (lastShownCompletion < 0)
            lastShownCompletion = lastCompletions.length-1;
        String sno = (String)(lastCompletions[lastShownCompletion]);
        toSetIn = sno.substring(lastBeginning.length()-1);
        StringBuffer alltext = new StringBuffer(comp.getText());
        int deletedChars = comp.getSelectionEnd() - comp.getSelectionStart();
        alltext.delete(comp.getSelectionStart(), comp.getSelectionEnd());
        int cp = comp.getCaretPosition() - deletedChars;
        alltext.insert(cp, toSetIn.substring(1));
        
        comp.setText(alltext.toString());
        comp.setCaretPosition(cp+toSetIn.length()-1);
        comp.select(cp, cp + sno.length() - lastBeginning.length());
        lastCaretPosition = comp.getCaretPosition();
        
    }

    public void keyTyped(KeyEvent e) {
        
        char ch = e.getKeyChar();
        if (Character.isLetter(ch)) {
            JTextComponent comp = (JTextComponent) e.getSource();
            if ((toSetIn != null) && (toSetIn.length() > 1) &&
                    (ch == toSetIn.charAt(1))) {
                
                toSetIn = toSetIn.substring(1);
                if (toSetIn.length() > 0) {
                    int cp = comp.getCaretPosition();
                    
                    
                    comp.select(cp + 1 - toSetIn.length(), cp);
                    lastBeginning = lastBeginning + ch;

                    e.consume();
                    lastCaretPosition = comp.getCaretPosition();

                    
                    

                    lastCompletions = findCompletions(lastBeginning, comp);
                    lastShownCompletion = 0;
                    for (int i = 0; i < lastCompletions.length; i++) {
                        Object lastCompletion = lastCompletions[i];
                        
                        if (((String)lastCompletion).endsWith(toSetIn)) {
                            lastShownCompletion = i;
                            break;
                        }

                    }
                    
                    if (toSetIn.length() < 2)
                        toSetIn = null;
                    return;
                }
            }

            if ((toSetIn != null) && ((toSetIn.length() <= 1) ||
                    (ch != toSetIn.charAt(1)))) {
                
                lastBeginning = lastBeginning + ch;
                Object[] completed = findCompletions(lastBeginning, comp);
                if ((completed != null) && (completed.length > 0)) {
                    lastShownCompletion = 0;
                    lastCompletions = completed;
                    String sno = (String) (completed[0]);
                    int lastLen = toSetIn.length() - 1;
                    toSetIn = sno.substring(lastBeginning.length() - 1);
                    String text = comp.getText();
                    
                    comp.setText(text.substring(0, lastCaretPosition - lastLen)
                            + toSetIn
                            + text.substring(lastCaretPosition));
                    comp.select(lastCaretPosition + 1 - lastLen,
                            lastCaretPosition + toSetIn.length() - lastLen);

                    lastCaretPosition = comp.getCaretPosition();
                    e.consume();
                    return;
                } else {
                    toSetIn = null;
                    return;
                }
            }


            StringBuffer currentword = getCurrentWord(comp);
            if (currentword == null)
                return;
            currentword.append(ch);

            Object[] completed = findCompletions(currentword.toString(), comp);

            int no = 0; 
            if ((completed != null) && (completed.length > 0)) {
                lastShownCompletion = 0;
                lastCompletions = completed;
                String sno = (String) (completed[no]);
                toSetIn = sno.substring(currentword.length() - 1);
                
                StringBuffer alltext = new StringBuffer(comp.getText());
                int cp = comp.getCaretPosition();
                alltext.insert(cp, toSetIn);
                
                comp.setText(alltext.toString());
                comp.setCaretPosition(cp);
                comp.select(cp + 1, cp + 1 + sno.length() - currentword.length());
                e.consume();
                lastCaretPosition = comp.getCaretPosition();
                lastBeginning = currentword.toString();
                return;
            }
        }
        
        toSetIn = null;
        lastCompletions = null;

    }


    protected Object[] findCompletions(String beginning, JTextComponent comp) {
        Object[] completed;
        if (completer.isNameField()) {
            int nameStatus = findNamePositionStatus(comp);
            if (nameStatus == ANY_NAME)
                completed = completer.complete(beginning);
            else if (nameStatus == FIRST_NAME)
                completed = completer.completeName(beginning, false);
            else
                completed = completer.completeName(beginning, true);
        }
        else
            completed = completer.complete(beginning);
        return completed;
    }

    protected StringBuffer getCurrentWord(JTextComponent comp) {
        StringBuffer res = new StringBuffer();
        String upToCaret;

        try {
            upToCaret = comp.getText(0, comp.getCaretPosition());
            
            
            
            if (!completer.isSingleUnitField()) {
                if ((comp.getCaretPosition() < comp.getText().length())
                        && !Character.isWhitespace(comp.getText().charAt(comp.getCaretPosition())))
                    return null;
                boolean found = false;
                int piv = upToCaret.length() - 1;
                while (!found && (piv >= 0)) {
                    if (Character.isWhitespace(upToCaret.charAt(piv)))
                        found = true;
                    else piv--;
                }
                
                
                res.append(upToCaret.substring(piv + 1));
            }
            
            
            else res.append(upToCaret);
            
        } catch (BadLocationException ex) {
        }

        return res;
    }

    final static int ANY_NAME = 0, FIRST_NAME = 1, LAST_NAME = 2;
    protected int findNamePositionStatus(JTextComponent comp) {
        String upToCaret;
        try {
            upToCaret = comp.getText(0, comp.getCaretPosition());
            
            upToCaret = upToCaret.substring(upToCaret.lastIndexOf(" and ")+1);
            int commaIndex = upToCaret.indexOf(',');
            if (commaIndex < 0)
                return ANY_NAME;
            else
                return FIRST_NAME;
            
        } catch (BadLocationException ex) {
            return ANY_NAME;
        }

    }

    public void focusGained(FocusEvent event) {
        if (nextFocusListener != null)
            nextFocusListener.focusGained(event);
    }

    public void focusLost(FocusEvent event) {
        if (lastCompletions != null) {
            JTextComponent comp = (JTextComponent)event.getSource();
            int selStart = comp.getSelectionStart();
            String text = comp.getText();
            comp.setText(text.substring(0, selStart) + text.substring(comp.getSelectionEnd()));
            comp.setCaretPosition(selStart);
            lastCompletions = null;
            lastShownCompletion = 0;
            lastCaretPosition = -1;
            toSetIn = null;
        }
        if (nextFocusListener != null)
            nextFocusListener.focusLost(event);
    }
}
