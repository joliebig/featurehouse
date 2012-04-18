

class Notepad {
	//Create the menu items
    private JMenuItem neW, opeN, savE, saveAS;
    //Create the buttons
    private JButton newButton, openButton, saveButton, saveAsButton;
	//Constructor of Notepad
	void MenuFileIOHook() {
		/**
		 *adding neW, opeN, savE, saveAS to the filE Menu,
		 *adding a small image icon to the menu item &
		 *adding separator between the menu item
		 */
		filE.add(neW    = new JMenuItem("New", new ImageIcon(this.getClass().getResource("images/new.gif"))));
		filE.add(opeN   = new JMenuItem("Open", new ImageIcon(this.getClass().getResource("images/open.gif"))));
		filE.add(savE   = new JMenuItem("Save", new ImageIcon(this.getClass().getResource("images/save.gif"))));
		filE.add(saveAS = new JMenuItem("Save As", new ImageIcon(this.getClass().getResource("images/saveAs.gif"))));

	}

	void ToolBarFileIOHook() {
		/**
		 *adding newButton, openButton, saveButton, saveAsButton, printButton, undoButton,
		 *redoButton, cutButton, copyButton, pasteButton, fontButton & aboutButton to the tool bar,
		 *adding a small image icon to the menu item &
		 *adding separator between the button
		 */
		toolBar.add(newButton   = new JButton(new ImageIcon(this.getClass().getResource("images/new.gif"))));
		toolBar.add(openButton  = new JButton(new ImageIcon(this.getClass().getResource("images/open.gif"))));
		toolBar.add(saveButton  = new JButton(new ImageIcon(this.getClass().getResource("images/save.gif"))));
		toolBar.add(saveAsButton= new JButton(new ImageIcon(this.getClass().getResource("images/saveAs.gif"))));
		toolBar.addSeparator();

		//adding a tool tip text to the button for descriping the image icon.
		newButton.setToolTipText("New");
		openButton.setToolTipText("Open");
		saveButton.setToolTipText("Save");
		saveAsButton.setToolTipText("Save As");
}

	Notepad(){
		/**
		 *allowing the neW       menu item to be selected by pressing ALT + N
		 *allowing the opeN      menu item to be selected by pressing ALT + O
		 *allowing the savE      menu item to be selected by pressing ALT + S
		 */
		neW.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK));
		opeN.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK));
		savE.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK));
		
		/**
		 *adding action listener for menu item: neW, opeN, savE, saveAS, exiT,
		 *redO, undO, copY, cuT, pastE, finD, findNexT, selectALL, lineWraP, fonT & abouT
		 *the actions was written @Actions.java
		 */
		neW.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.neW();
			}
		});
		opeN.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.opeN();
			}
		});
		savE.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.savE();
			}
		});
		saveAS.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.saveAs();
			}
		});
		
		/**
		 *adding action listener for the button in the tool bar: newButton, openButton,
		 *saveButton, saveAsButton, printButton, redoButton, undoButton, copyButton,
		 *cutButton, pasteButton, findButton, selectALL, lineWraP, fontButton & aboutButton
		 *the actions was written @Actions.java
		 */
		newButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.neW();
			}
		});
		openButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.opeN();
			}
		});
		saveButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.savE();
			}
		});
		saveAsButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.saveAs();
			}
		});
 	}
}			
