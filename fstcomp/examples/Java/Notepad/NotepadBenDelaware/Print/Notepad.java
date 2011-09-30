

/**
 *A PUBLIC CLASS FOR NOTEPAD.JAVA
 */
class Notepad{
	//Create the menu items
    private JMenuItem prinT;
    //Create the buttons
    private JButton printButton; 
    //Create Scroll pane (JScrollPane) for the JTextArea
    //Constructor of Notepad

    void  MenuPrintHook(){
	filE.add(prinT  = new JMenuItem("Print", new ImageIcon(this.getClass().getResource("images/print.gif"))));
		/**
		 *allowing the prinT     menu item to be selected by pressing ALT + P
		 */
		prinT.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P, ActionEvent.CTRL_MASK));

}
     void ToolBarPrintHook(){
		/**
		 *adding printButton,
		 *adding a small image icon to the menu item &
		 *adding separator between the button
		 */
		toolBar.add(printButton = new JButton(new ImageIcon(this.getClass().getResource("images/print.gif"))));
		toolBar.addSeparator();
		
		//adding a tool tip text to the button for descriping the image icon.
		printButton.setToolTipText("Print");
}

   Notepad(){

		prinT.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.prinT();
			}
		});
		/**
		 *adding action listener for the button in the tool bar: printButton,
		 *the actions was written @Actions.java
		 */
		printButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.prinT();
			}
		});
	}
}			
