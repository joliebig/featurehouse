

public class GuiBoard {
    public void update(Board b) {
        original(b);
        Field field;
        JLabel label;
        for (int boxNr = 0; boxNr < Field.POSSIBILITIES; boxNr++) {
            for (int fieldNr = 0; fieldNr < Field.POSSIBILITIES; fieldNr++) {
                field = b.getField(Structure.BOX, boxNr, fieldNr);
                label = boxes[boxNr].getLabel(fieldNr);
                label.setOpaque(true);
                if (field.isInitialSet()) {
                    label.setBackground(Color.BLACK);
                    label.setForeground(Color.ORANGE);
                } else if (field.isSet()) {
                    label.setForeground(Color.BLACK);
                    label.setBackground(Color.ORANGE);
                } else {
                    label.setBackground(Color.WHITE);
                }
            }
        }
    }
}