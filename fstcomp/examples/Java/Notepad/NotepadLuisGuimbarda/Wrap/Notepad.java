

class Notepad{
    private JCheckBoxMenuItem lineWraP;
    public JCheckBoxMenuItem getLineWrap(){
        return lineWraP;
    }
    Notepad(){
        /**
         *adding lineWraP to the formaT Menu
         */
        formaT.add(lineWraP = new JCheckBoxMenuItem("Line Wrap"));
        lineWraP.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                actions.lineWraP();
            }
        });
        /**
         *Setting the Line Wrap & Wrap Style Word features are true
         */
        textArea.setLineWrap(true);
        textArea.setWrapStyleWord(true);
    }
}
