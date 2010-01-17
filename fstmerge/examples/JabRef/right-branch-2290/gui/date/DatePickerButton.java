











package net.sf.jabref.gui.date ;

import java.awt.event.* ;
import java.awt.*;
import javax.swing.* ;

import com.michaelbaranov.microba.calendar.* ;
import net.sf.jabref.* ;

import java.util.*;

public class DatePickerButton implements ActionListener {
    private DatePicker datePicker = new DatePicker();
    private JPanel panel = new JPanel();
    private FieldEditor editor;

    public DatePickerButton(FieldEditor pEditor) {
        datePicker.showButtonOnly(true);
        datePicker.addActionListener(this);
        datePicker.setShowTodayButton(true);
        panel.setLayout(new BorderLayout());
        panel.add(datePicker, BorderLayout.WEST);
        editor = pEditor;
    }

    public void actionPerformed(ActionEvent e) {
        Date date = datePicker.getDate();
        if (date != null) {
            editor.setText(Util.easyDateFormat(date));
            
            new FocusRequester(editor.getTextComponent());
        }
    }

    public JComponent getDatePicker() {
        
        return panel;
    }
}
