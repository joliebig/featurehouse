import javax.swing.*;
import java.awt.*;
import Jakarta.util.*;
import java.util.*;

class pattern {

    public JComponent draw (int several) {
       // Step 1: create horizontal panel and add terms that
       //         have a non-null graphic
   JPanel panel = new JPanel();
    panel.setLayout( new FlowLayout(FlowLayout.LEFT) );
   // panel.setBorder( BorderFactory.createEtchedBorder()); //no panel required, spoils the visual

   if (var==null)
           Util.fatalError(" var null for " + name);

       // Step 2: add terms with non-null graphic
       Iterator i = terms.iterator();
        while ( i.hasNext() ) {
          term t = (term) i.next();
          JComponent j = t.draw(several);

          if (j!=null)
             panel.add( t.setWidget(j) );
       }
        return panel;
    }

    /*
    returns true if pattern has non-optional subterms
    used by production when deciding whether to display
    a checkbox for an optional single-pattern production
    */

    boolean hasNonOpt(){
        Iterator i = terms.iterator();
        while(i.hasNext()){
            term t = (term)i.next();
            if (t.prod != null && t.prod.type != production.opt )//non-optional production
                    return true;
            else
                if(t.getClass().getName().equals("guidsl.prim"))//non-optional primitive
                    return true;
        }
        return false;
    }
}
