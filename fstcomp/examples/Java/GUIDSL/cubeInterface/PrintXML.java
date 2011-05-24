
import java.util.*;
import Jakarta.util.*;


/**
 * This class is used as a visitor in the Visitor pattern implemented in the GUI dsl paser
 * traverses through every node in the tree and prints out an XML representation.
 */
public class PrintXML implements GVisitor {
   Stack s;
   StringBuffer str = new StringBuffer();

   public void outln(String s) {
    //System.out.println(s);
    str.append(s);
    str.append("\n");
   }
   public void out(String s) {
    str.append(s);
    //System.out.print(s);
   }

   public void action( grammar n ) {
      s = new Stack();
      //outln("------------------------------------");
      outln("<grammar name='" + n.name+"'>");
      n.traverse(((PrintXML) this));
      while (!s.empty()) {
        production p = (production) s.pop();
        action(p);
      }
      outln("</grammar>");
      //outln("------------------------------------");
   }
   public void action( optprim n ) {
        out(" <primitive type='optional '>" + n.name +"</primitive> ");
        //out(" [" + n.name +"] ");
   }
   public void action( optprod n ) {
        out(" <production type='optional'>" + n.name +"</production> ");
      //out(" [" + n.name +"] ");
      s.push(n.prod);
   }
   public void action( pattern n ) {
      out(" ");
      outln("<pattern name='"+ n.name+"'>");
      n.traverse(((PrintXML) this));
      //outln(":: " + n.name);
      outln("</pattern>");
   }
   public void action( plus n ) {
      out(" <production_ref type='plus'>" + n.name + "</production_ref> ");
      s.push(n.prod);
   }
   public void action( prim n ) {
      out(" <primitive type='required'>" + n.name + "</primitive> " );
   }
   public void action( prod n ) {
      out(" <production_ref type='choose1'>" + n.name + "</production_ref> " );
      s.push(n.prod);
   }
   public void action( star n ) {
      out("  <production_ref type='star'>" + n.name +"</production_ref> ");
      s.push(n.prod);
   }
   public void action( production n ) {
      outln("<production name='"+n.name + "' type='" + n.getType() + "'>" );
      n.traverse(((PrintXML) this));
      outln("</production>");
   }
   public void action( term n ) {
      Util.fatalError("should never call printgs.action(term)");
   }
   public void action( variable n ) {
      Util.fatalError("should never call printgs.action(variable)");
   }

   public String getXMLString(){
        grammar.current.visit( this );
        return str.toString();
        //return XMLUtils.formatXMLStr(str.toString());
   }
}
