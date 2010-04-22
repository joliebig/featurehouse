import java.awt.event.ActionListener;
import java.awt.event.MouseListener;

import javax.swing.JComponent;

public class ListenerFactory {
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
}
