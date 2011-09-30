

class Notepad{
    //Create the menu items
    private JMenuItem finD, findNexT;
    //Create the buttons
    private JButton findButton;

    Notepad(){
        /**
         *adding finD, findNexT &
         *adding a small image icon to the menu item
         */
        ediT.add(finD = new JMenuItem("Find", new ImageIcon(this.getClass().getResource("images/find.gif"))));
        ediT.add(findNexT = new JMenuItem("Find Next"));

        /**
         *allowing the finD      menu item to be selected by pressing ALT + F
         *allowing the findNexT  menu item to be selected by pressing ALT + F3
         */
        finD.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, ActionEvent.CTRL_MASK));
        findNexT.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F3, ActionEvent.CTRL_MASK));
        /**
         *adding findButton &
         *adding a small image icon to the menu item
         */
        toolBar.add(findButton  = new JButton(new ImageIcon(this.getClass().getResource("images/find.gif"))));

        //adding a tool tip text to the button for descriping the image icon.
        findButton.setToolTipText("Find");

        finD.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                actions.finD();
            }
        });
        findNexT.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                actions.findNexT();
            }
        });
        /**
         *adding action listener for the button in the tool bar: findButton
         *the actions was written @Actions.java
         */
        findButton.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                actions.finD();
            }
        });

    }
    private void conditionalAddToolbar(Container cp){
        cp.add("North", toolBar = new JToolBar("Tool Bar"));
    }
}
