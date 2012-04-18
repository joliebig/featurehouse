

class Notepad{
    //Create the menu items
    private JMenuItem cuT, copY, pastE;
    //Create the buttons
    private JButton cutButton, copyButton, pasteButton;

    Notepad(){
        /**
         *adding cuT, copY, pastE &
         *adding a small image icon to the menu item
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

        /**
         *adding newButton, openButton, saveButton, saveAsButton, printButton, undoButton,
         *redoButton, cutButton, copyButton, pasteButton, fontButton & aboutButton to the tool bar,
         *adding a small image icon to the menu item &
         *adding separator between the button
         */
        toolBar.add(cutButton   = new JButton(new ImageIcon(this.getClass().getResource("images/cut.gif"))));
        toolBar.add(copyButton  = new JButton(new ImageIcon(this.getClass().getResource("images/copy.gif"))));
        toolBar.add(pasteButton = new JButton(new ImageIcon(this.getClass().getResource("images/paste.gif"))));
        //adding a tool tip text to the button for descriping the image icon.
        cutButton.setToolTipText("Cut");
        copyButton.setToolTipText("Copy");
        pasteButton.setToolTipText("Paste");


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

        /**
         *adding action listener for the button in the tool bar: copyButton,
         *cutButton & pasteButton
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
    private void conditionalAddToolbar(Container cp){
        cp.add("North", toolBar = new JToolBar("Tool Bar"));
    }
}
