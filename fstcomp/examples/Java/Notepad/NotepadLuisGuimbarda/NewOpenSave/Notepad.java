

class Notepad{
    //Create the menu items
    private JMenuItem neW, opeN, savE, saveAS;

    Notepad(){
        /**
         *adding neW, opeN, savE, saveAS &
         *adding a small image icon to the menu item
         */
        filE.add(neW    = new JMenuItem("New", new ImageIcon(this.getClass().getResource("images/new.gif"))));
        filE.add(opeN   = new JMenuItem("Open", new ImageIcon(this.getClass().getResource("images/open.gif"))));
        filE.add(savE   = new JMenuItem("Save", new ImageIcon(this.getClass().getResource("images/save.gif"))));
        filE.add(saveAS = new JMenuItem("Save As", new ImageIcon(this.getClass().getResource("images/saveAs.gif"))));

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
    }
}
