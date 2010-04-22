/**
 *
 *
 */
public class Loader {

    protected Gui gui;

    protected static Loader loader;

    public static void main(String[] args) {
        loader = new Loader();
        loader.load();
    }

    public void load() {
        BoardManager bm = new BoardManager();
        ListenerFactory listenerFactory = new ListenerFactory(bm);
        gui = new Gui(listenerFactory);
        bm.registerSudokuView(gui);
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                gui.createAndShowGUI();
            }
        });
    }

}
