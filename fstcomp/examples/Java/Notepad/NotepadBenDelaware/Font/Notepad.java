

/**
 *A PUBLIC CLASS FOR NOTEPAD.JAVA font properties
 */
class Notepad {

    //for using the methods in these classes
    private JMenu formaT;
    //Create the menu items
    private JMenuItem fonT;
    private JCheckBoxMenuItem lineWraP;
    //Create the Tool Bar that contains the JButton
    private JButton fontButton;
    
    //for using lineWrap & textArea @Actions.java
    public JCheckBoxMenuItem getLineWrap(){
	return lineWraP;
    }

    void MenuFormatHook(){
	//adding file, edit, view, format, help to the menu bar
	Menubar.add(formaT = new JMenu("Format"));
	/**
	 *adding lineWraP & fonT to the formaT Menu,
	 *adding abouT to the helP Menu & 
	 *adding a samll image icon to the menu item
	 */
	formaT.add(lineWraP = new JCheckBoxMenuItem("Line Wrap"));
	formaT.add(fonT = new JMenuItem("Font", new ImageIcon(this.getClass().getResource("images/font.gif"))));
	/**
		 *allowing the format menu to be selected by pressing ALT + O
		 */
		formaT.setMnemonic('o');

}

	void ToolBarFormatHook(){
		/**
		 *adding fontButton to the tool bar,
		 *adding a small image icon to the menu item &
		 *adding separator between the button
		 */
		toolBar.add(fontButton  = new JButton(new ImageIcon(this.getClass().getResource("images/font.gif"))));
		toolBar.addSeparator();
		
		//adding a tool tip text to the button for descriping the image icon.
		fontButton.setToolTipText("Font");
}

    //Constructor of Notepad
    Notepad(){
		/**
		 *adding action listener for menu item: lineWraP, fonT
		 */
		lineWraP.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.lineWraP();
			}
		});
		fonT.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.fonT();
			}
		});

		/**
		 *adding action listener for the button in the tool bar: lineWraP, fontButton 
		 *the actions was written @Actions.java
		 */
		fontButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.fonT();
			}
		});
	}
}			
