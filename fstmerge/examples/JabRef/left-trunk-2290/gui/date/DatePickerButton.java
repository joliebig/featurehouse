











package net.sf.jabref.gui.date ;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Date;

import javax.swing.JComponent;
import javax.swing.JPanel;

import net.sf.jabref.FieldEditor;
import net.sf.jabref.FocusRequester;
import net.sf.jabref.Util;

import com.michaelbaranov.microba.calendar.DatePicker;

public class DatePickerButton implements ActionListener {
    private DatePicker datePicker = new DatePicker();
    private JPanel panel = new JPanel();
    private FieldEditor editor;

    public DatePickerButton(FieldEditor pEditor) {
        datePicker.showButtonOnly(true);
        datePicker.addActionListener(this);
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
