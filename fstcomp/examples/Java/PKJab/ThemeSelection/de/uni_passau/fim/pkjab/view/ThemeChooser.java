package de.uni_passau.fim.pkjab.view;

import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JTextField;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.BorderFactory;
import javax.swing.JColorChooser;

import java.awt.GridLayout;
import java.awt.Font;
import java.awt.Color;
import java.awt.Dimension;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

class ThemeChooser extends JDialog {
	
	private AbstractTheme theme;
	
	private static final String BGR_COLOR = "Hintergrundfarbe: ";
	
	private static final String FGR_COLOR = "Vordergrundfarbe: ";
	
	private static final String FONT_FAMILY = "Schriftart: ";
	
	 private static final String BUTTON_OK = "OK";
     
     private static final String BUTTON_CANCEL = "Abbrechen";
     
     private static final String BOLD = "fett";
     
     private static final String ITALIC = "kursiv";
     
     private static final String FONT_SIZE = "Schriftgroesse: ";
     
     private Color nBgr;
     
     private Color nFgr;
	
	ThemeChooser(JFrame owner, String title, boolean modal) {
            super(owner, title, modal);
            
            final DefaultTheme defaultTheme = new DefaultTheme();
            
            final Dimension colorDim = new Dimension(20, 20);
            
            nBgr = defaultTheme.getBackgroundColor();
            nFgr = defaultTheme.getForegroundColor();
            
            final JButton bgrColorBtn = new JButton();
            bgrColorBtn.setMinimumSize(colorDim);
            bgrColorBtn.setBackground(defaultTheme.getBackgroundColor());
            bgrColorBtn.addActionListener(new ActionListener() {
            	 public void actionPerformed(ActionEvent e) {
            		 
            		 Color newColor = JColorChooser.showDialog(
                             null,
                             BGR_COLOR,
                             defaultTheme.getBackgroundColor());
            		 bgrColorBtn.setBackground(newColor);
            		 nBgr = newColor;
            	 }
            });
            
            final JButton fgrColorBtn = new JButton();
            fgrColorBtn.setMinimumSize(colorDim);
            fgrColorBtn.setBackground(defaultTheme.getForegroundColor());
            fgrColorBtn.addActionListener(new ActionListener() {
           	 public void actionPerformed(ActionEvent e) {
           		 
           		 Color newColor = JColorChooser.showDialog(
                            null,
                            FGR_COLOR,
                            defaultTheme.getForegroundColor());
           		 fgrColorBtn.setBackground(newColor);
           		 nFgr = newColor;
           	 }
           });
            
            final JTextField fontNameField = new JTextField(defaultTheme.getFont().getFontName());
            final JCheckBox boldCheckBox = new JCheckBox(BOLD);
            final JCheckBox italicCheckBox = new JCheckBox(ITALIC);
            final JTextField fontSizeField = new JTextField();
            fontSizeField.setText(defaultTheme.getFont().getSize() + "");
            
            JPanel contentPane = new JPanel();
            contentPane.setLayout(new GridLayout(7, 2, 5, 10));
            
            contentPane.add(new JLabel(BGR_COLOR));
            contentPane.add(bgrColorBtn);
            
            contentPane.add(new JLabel(FGR_COLOR));
            contentPane.add(fgrColorBtn);
            
            contentPane.add(new JLabel(FONT_FAMILY));
            contentPane.add(fontNameField);
            
            contentPane.add(new JLabel(" "));
            contentPane.add(boldCheckBox);
            
            contentPane.add(new JLabel(" "));
            contentPane.add(italicCheckBox);
            
            contentPane.add(new JLabel(FONT_SIZE));
            contentPane.add(fontSizeField);
            
            final JButton okButton = new JButton(BUTTON_OK);
            okButton.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                	
                	int fontStyle = 0;
                	if (boldCheckBox.isSelected())
                		fontStyle |= Font.BOLD;
                	if (italicCheckBox.isSelected())
                		fontStyle |= Font.ITALIC;
                	
                	Font font = new Font(fontNameField.getText(), fontStyle, 
                			Integer.parseInt(fontSizeField.getText()));
                	
                    theme = new IndividualTheme(nBgr, nFgr, font);
                    setVisible(false);
                }
            });
            
            final JButton cancelButton = new JButton(BUTTON_CANCEL);
            cancelButton.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    setVisible(false);
                }
            });
            
            contentPane.add(okButton);
            contentPane.add(cancelButton);
            
            contentPane.setBorder(BorderFactory.createEmptyBorder(10, 20, 5, 20));
            setContentPane(contentPane);
            getRootPane().setDefaultButton(okButton);
            
            pack();
            setLocationRelativeTo(null);
            setVisible(true);
	}
	
	AbstractTheme getTheme() {
		return theme;
	}
	
}