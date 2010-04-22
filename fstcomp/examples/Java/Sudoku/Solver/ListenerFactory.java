

public class ListenerFactory {

    public ActionListener getSolutionHintListener() {
        return new SolutionHintListener(bm);
    }
}