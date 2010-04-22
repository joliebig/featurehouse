

public class ListenerFactory {

    public ActionListener getSaveStateListener() {
        return new SaveStateListener(bm);
    }

    public ActionListener getLoadStateListener() {
        return new LoadStateListener(bm);
    }
}