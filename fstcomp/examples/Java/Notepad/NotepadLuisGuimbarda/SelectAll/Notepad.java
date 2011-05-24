

class Notepad{
    private JMenuItem selectALL;

    Notepad(){
        /**
         *adding selectALL to the ediT Menu
         */
        ediT.add(selectALL= new JMenuItem("Select All"));

        /**
         *allowing the selectAll menu item to be selected by pressing ALT + A
         */
        selectALL.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A, ActionEvent.CTRL_MASK));

        selectALL.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                actions.selectALL();
            }
        });
    }
}
