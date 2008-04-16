import Jakarta.util.*;

class Bvar {

   public node eharvest () {
      String name = tok[0].getTokenName();
      // check to see if this identifier was harvested already
      // if not, we have an error
      if (variable.find(name) == null)
         Util.error( "variable " + name + " referenced in a constraint, but" +
                     " not defined in grammar");
      return new bterm( name );
    }
}
