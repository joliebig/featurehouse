

class Notepad{
    //Create the menu items
    private JMenuItem finD, findNexT;
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
    }
}
