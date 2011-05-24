

public class ListenerFactory {

    public ActionListener getUndoListener() {
        return new UndoListener(bm);
    }
}