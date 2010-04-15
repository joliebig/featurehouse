

public class Gui {

	protected JButton colorButton;
	
	protected Color color = Color.BLACK;

	protected void addScrollPane(JFrame frame, JScrollPane scrollPane) {
		GridBagConstraints c = new GridBagConstraints();
		
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 2;
		frame.add(scrollPane, c);	
		
		colorButton = new JButton();
		colorButton.setText("Farbe");
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 0.2;
		c.weighty = 0;
		c.gridx = 1;
		c.gridy = 1;
		c.gridwidth = 1;
		add(colorButton, c);
		
		colorButton.addActionListener(new ActionListener() {
			
			public void actionPerformed(ActionEvent arg0) {
				color = JColorChooser.showDialog(window, "Choose a color", color);		
			}
		});
	}
	
	protected void addTextField(JFrame frame, final JTextField inputField) {
		GridBagConstraints c = new GridBagConstraints();
		
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 0.8;
		c.weighty = 0;
		c.gridx = 0;
		c.gridy = 1;
		frame.add(inputField, c);
		
		inputField.addKeyListener(new KeyListener() {
			
			public void keyTyped(KeyEvent arg0) {
				// TODO Auto-generated method stub
				
			}
			
			public void keyReleased(KeyEvent arg0) {
				// TODO Auto-generated method stub
				
			}
			
			public void keyPressed(KeyEvent arg0) {
				if (arg0.getKeyCode() == KeyEvent.VK_ENTER
						&& inputField.getText().length() > 0) {
					chatClient.send(inputField.getText(), color);
					inputField.setText("");
				}
				
			}
		});
	}

	public void newChatLine(String line, Color color) {
		StyleContext sc = StyleContext.getDefaultStyleContext();
	    AttributeSet aset = sc.addAttribute(SimpleAttributeSet.EMPTY,
	        StyleConstants.Foreground, color);

	    int len = outputTextbox.getDocument().getLength(); // same value as
	                       // getText().length();
	    outputTextbox.setCaretPosition(len); // place caret at the end (with no selection)
	    outputTextbox.setCharacterAttributes(aset, false);
	    outputTextbox.replaceSelection(line); // there is no selection, so inserts at caret
	    
	    try {
			outputTextbox.getDocument().insertString(outputTextbox.getDocument().getLength(),
					 								 line, aset);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}
}