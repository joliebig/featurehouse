import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedList;
import javax.swing.JOptionPane;

public class SolutionHintListener implements ActionListener {

    protected BoardManager bm;

    public SolutionHintListener(BoardManager bm) {
        this.bm = bm;
    }

    /**
     * @param e
     *            ActionEvent
     *
     */
    public void actionPerformed(ActionEvent e) {
        Thread worker = new Thread() {
            public void run() {
                if (!bm.solutionHint())
                    JOptionPane.showMessageDialog(null, "Sudoku not solvable!");
            }
        };
        worker.start();
    }
}