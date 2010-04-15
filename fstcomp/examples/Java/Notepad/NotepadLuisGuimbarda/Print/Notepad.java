

class Notepad {
    //Create the menu items
    private JMenuItem prinT;

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

        prinT.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                actions.prinT();
            }
        });
    }
}
