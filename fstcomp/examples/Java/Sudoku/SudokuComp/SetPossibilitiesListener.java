

import java.awt.event.ActionEvent; 
import java.awt.event.ActionListener; 
import javax.swing.JOptionPane; 

public  class  SetPossibilitiesListener  implements ActionListener {
	

    final static Object[] SELECTION_VALUES = { "4", "9", "16", "25", "36" };

	

    protected BoardManager boardManager;

	

    public SetPossibilitiesListener(BoardManager bM) {
        this.boardManager = bM;
    }

	


    public void actionPerformed(ActionEvent arg0) {
        Object poss = JOptionPane.showInputDialog(null, "Select number of possibilities", "", JOptionPane.QUESTION_MESSAGE, null, SELECTION_VALUES, String.valueOf(Field.POSSIBILITIES));
        if (null != poss)
            boardManager.setPossibilities(Integer.valueOf((String) poss));
    }


}
