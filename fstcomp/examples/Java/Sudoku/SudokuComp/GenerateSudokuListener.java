

import java.awt.event.ActionEvent; 
import java.awt.event.ActionListener; 
import java.util.LinkedList; 
import javax.swing.JOptionPane; 

public  class  GenerateSudokuListener  implements ActionListener {
	

    protected BoardManager bm;

	
    
    protected SudokuGenerator sGen;

	

    public GenerateSudokuListener(BoardManager bm) {
        this.bm = bm;
        sGen = new SudokuGenerator();
    }

	

    /**
     * @param e
     *            ActionEvent
     *
     */
    public void actionPerformed(ActionEvent e) {
             
        Thread worker = new Thread() {
            public void run() {
                bm.setBusy(true);
                Board b = sGen.generate();
                bm.loadSudoku(b);
                bm.setBusy(false);
            }
        };
        worker.start();
        
    }


}
