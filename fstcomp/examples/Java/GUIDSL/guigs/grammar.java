import javax.swing.*;
import Jakarta.util.*;

class grammar {
    public JComponent draw (int several) {

        // Step 1: do so consistency checking

        if (rootProduction.type != production.norm)
            Util.fatalError( "root production should be choose1" );

        // Step 2: return graphic of root production

        return setWidget(rootProduction.draw(0));
    }
}
