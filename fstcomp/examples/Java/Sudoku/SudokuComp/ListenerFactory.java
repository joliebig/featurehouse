import java.awt.event.ActionListener; 
import java.awt.event.MouseListener; 

import javax.swing.JComponent; 

public   class  ListenerFactory {
	
    protected BoardManager bm;

	

    public ListenerFactory(BoardManager bm) {
        this.bm = bm;
    }

	

    public ActionListener getLoadFileListener() {
        return new LoadFileListener(bm);
    }

	

    public MouseListener getPopupMenuListener(GuiField field) {
        return new GuiFieldPopupMenuListener(field,bm);
    }

	

    public ActionListener getSaveStateListener() {
        return new SaveStateListener(bm);
    }

	

    public ActionListener getLoadStateListener() {
        return new LoadStateListener(bm);
    }

	

    public ActionListener getUndoListener() {
        return new UndoListener(bm);
    }

	

    public ActionListener getSolutionHintListener() {
        return new SolutionHintListener(bm);
    }

	

    public ActionListener getGenerateSudokuListener() {
        return new GenerateSudokuListener(bm);
    }

	
    public ActionListener getSetPossibilitiesListener() {
        return new SetPossibilitiesListener(bm);
    }


}
