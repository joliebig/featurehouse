import java.awt.Color;
import javax.swing.BorderFactory;
import javax.swing.border.Border;

public class GuiFieldPopupMenuListener {

    private static final int EMPH_BORDER_THICKNESS = 4;
    Border oldBorder;

    public void mouseEntered(MouseEvent arg0) {
        if (!boardManager.getField(Structure.BOX, field.getBoxIndex(),
                                   field.getFieldIndex()).isInitialSet()) {
            oldBorder = field.getBorder();
            field.setBorder(BorderFactory.createLineBorder(Color.ORANGE,
                            EMPH_BORDER_THICKNESS));
        }

    }

    public void mouseExited(MouseEvent arg0) {
        if (!boardManager.getField(Structure.BOX, field.getBoxIndex(),
                                   field.getFieldIndex()).isInitialSet()) {
            field.setBorder(oldBorder);
        }

    }

}