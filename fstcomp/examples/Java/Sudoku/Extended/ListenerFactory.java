

public class ListenerFactory {
    public ActionListener getSetPossibilitiesListener() {
        return new SetPossibilitiesListener(bm);
    }
}