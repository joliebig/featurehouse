

class Notepad{
    //Create the Font menu item
    private JMenuItem fonT;

    //Constructor of Notepad
    Notepad(){
        /**
         *adding fonT to the formaT Menu &
         *adding a samll image icon to the menu item
         */
        formaT.add(fonT = new JMenuItem("Font", new ImageIcon(this.getClass().getResource("images/font.gif"))));


        fonT.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                actions.fonT();
            }
        });
    }
}
