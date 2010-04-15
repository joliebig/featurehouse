

class Notepad {
    //Create the menu items
    private JMenuItem prinT;
    private JButton printButton;

    Notepad(){
        /**
         *adding prinT &
         *adding a small image icon to the menu item
         */
        filE.add(prinT  = new JMenuItem("Print", new ImageIcon(this.getClass().getResource("images/print.gif"))));

        /**
         *allowing the prinT     menu item to be selected by pressing ALT + P
         */
        prinT.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P, ActionEvent.CTRL_MASK));
        /**
         *adding printButton &
         *adding a small image icon to the menu item
         */
        toolBar.add(printButton = new JButton(new ImageIcon(this.getClass().getResource("images/print.gif"))));

        //adding a tool tip text to the button for descriping the image icon.
        printButton.setToolTipText("Print");


        prinT.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                actions.prinT();
            }
        });
        /**
         *adding action listener for the button in the tool bar: newButton, openButton,
         *saveButton, saveAsButton, printButton, redoButton, undoButton, copyButton,
         *cutButton, pasteButton, findButton, selectALL, lineWraP, fontButton & aboutButton
         *the actions was written @Actions.java
         */
        printButton.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                actions.prinT();
            }
        });

    }
    private void conditionalAddToolbar(Container cp){
        cp.add("North", toolBar = new JToolBar("Tool Bar"));
    }

}
