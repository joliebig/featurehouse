

/**
 *A PUBLIC CLASS FOR NOTEPAD.JAVA
 */
class Notepad {

	//Create the menu items
	private JMenuItem cuT, copY, pastE, selectALL;
	private JCheckBoxMenuItem lineWraP;
	//Create the Tool Bar that contains the JButton
    private JButton cutButton, copyButton, pasteButton;
    //Constructor of Notepad

    void MenuCutPasteHook(){
	//set the title for Notepad and set the size for it.
		/**
		 *adding cuT, copY & pastE to the ediT Menu,
		 *adding a small image icon to the menu item &
		 *adding separator between the menu item
		 */
		ediT.add(cuT  = new JMenuItem("Cut",  new ImageIcon(this.getClass().getResource("images/cut.gif"))));
		ediT.add(copY = new JMenuItem("Copy", new ImageIcon(this.getClass().getResource("images/copy.gif"))));
		ediT.add(pastE= new JMenuItem("Paste",new ImageIcon(this.getClass().getResource("images/paste.gif"))));
		/**
		 *allowing the cuT       menu item to be selected by pressing ALT + X
		 *allowing the copY      menu item to be selected by pressing ALT + C
		 *allowing the pastE     menu item to be selected by pressing ALT + V
		 */
		cuT.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X, ActionEvent.CTRL_MASK));
		copY.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, ActionEvent.CTRL_MASK));
		pastE.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V, ActionEvent.CTRL_MASK));


}

    void MenuSelectAllHook(){
	ediT.add(selectALL= new JMenuItem("Select All"));
    	selectALL.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A, ActionEvent.CTRL_MASK));
}

    void ToolBarCutPasteHook(){
		/**
		 *adding cutButton, copyButton & pasteButton to the tool bar,
		 *adding a small image icon to the menu item &
		 *adding separator between the button
		 */
		toolBar.add(cutButton   = new JButton(new ImageIcon(this.getClass().getResource("images/cut.gif"))));
		toolBar.add(copyButton  = new JButton(new ImageIcon(this.getClass().getResource("images/copy.gif"))));
		toolBar.add(pasteButton = new JButton(new ImageIcon(this.getClass().getResource("images/paste.gif"))));
		toolBar.addSeparator();

		//adding a tool tip text to the button for descriping the image icon.
		cutButton.setToolTipText("Cut");
		copyButton.setToolTipText("Copy");
		pasteButton.setToolTipText("Paste");
}

    Notepad(){
		
		/**
		 *adding action listener for menu item: neW, opeN, savE, saveAS, prinT, exiT,
		 *redO, undO, copY, cuT, pastE, finD, findNexT, selectALL, lineWraP, fonT & abouT
		 *the actions was written @Actions.java
		 */
		cuT.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.cuT();
			}
		});
		copY.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.copY();
			}
		});
		pastE.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.pastE();
			}
		});
		selectALL.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.selectALL();
			}
		});

		/**
		 *adding action listener for the button in the tool bar: newButton, openButton,
		 *saveButton, saveAsButton, printButton, redoButton, undoButton, copyButton,
		 *cutButton, pasteButton, findButton, selectALL, lineWraP, fontButton & aboutButton
		 *the actions was written @Actions.java
		 */
		cutButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.cuT();
			}
		});
		copyButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.copY();
			}
		});
		pasteButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.pastE();
			}
		});
	}
}			
