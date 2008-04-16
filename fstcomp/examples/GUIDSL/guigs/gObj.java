import javax.swing.*;
import Jakarta.util.*;

class gObj {
    public JComponent draw (int several) {
       Util.fatalError( "gObj.draw should never be called");
       return null;
    }

   JComponent setWidget( JComponent w ) {
      variable v = (variable) variable.Vtable.get(name);
      if (v==null)
         Util.fatalError("term " + name + " undefined");
      v.widget = w;
      return w;
   }
}
