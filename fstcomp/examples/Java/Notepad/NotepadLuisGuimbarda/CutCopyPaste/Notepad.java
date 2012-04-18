

class Notepad{
    //Create the menu items
    private JMenuItem cuT, copY, pastE;


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

    }
}
