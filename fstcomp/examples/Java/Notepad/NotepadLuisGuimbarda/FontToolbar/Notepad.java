

class Notepad{
    //Create the Font menu item
    private JMenuItem fonT;
    //Create the buttons
    private JButton fontButton;

        //Constructor of Notepad
    Notepad(){
        /**
         *adding fonT to the formaT Menu &
         *adding a samll image icon to the menu item
         */
        formaT.add(fonT = new JMenuItem("Font", new ImageIcon(this.getClass().getResource("images/font.gif"))));

        /**
         *adding fontButton to the tool bar &
         *adding a small image icon to the menu item
         */
        toolBar.add(fontButton  = new JButton(new ImageIcon(this.getClass().getResource("images/font.gif"))));

        //adding a tool tip text to the button for descriping the image icon.
        fontButton.setToolTipText("Font");

        fonT.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                actions.fonT();
            }
        });
        fontButton.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                actions.fonT();
            }
        });
    }
    private void conditionalAddToolbar(Container cp){
        cp.add("North", toolBar = new JToolBar("Tool Bar"));
    }

}
