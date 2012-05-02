import java.awt.event.ActionEvent; 
import java.awt.event.ActionListener; 

public  class  UndoListener  implements ActionListener {
	

    private BoardManager bm;

	

    public UndoListener(BoardManager bm) {
        this.bm = bm;
    }

	

    /**
     * @param e
     *            ActionEvent
     *
     */
    public void actionPerformed(ActionEvent e) {
        bm.undo();
    }


}
