// extension of the visitor that contains gui-specific code

import java.io.*;

class enterGspec     {

//processes hidden and tab annotations in the input file
   public void processOptid( ) {
      if (opt.equals("hidden")) {
         currentVar.hidden = true;
            foundOpt = true;
         return;
      }
        if (opt.equals("tab")) {
           currentVar.tab = true;
            foundOpt = true;
            return;
       }
        original();
   }

//reassigns the variable fields disp and help new values if encountered in the input file
    public void processStrlit() {
      if (opt.equals("disp")) {
         currentVar.disp = optVal.substring(1,optVal.length()-1);
        variable other =((variable)variable.Vtable.get("_"+currentVar.name));
         if (other != null)
            other.disp = optVal.substring(1,optVal.length()-1);
        foundOpt = true;
         return;
      }
      if (opt.equals("help")){
         currentVar.help= optVal.substring(1,optVal.length()-1);
         variable other =((variable)variable.Vtable.get("_"+currentVar.name));
         if (other != null)
            other.help = optVal.substring(1,optVal.length()-1);
         foundOpt = true;
         return;
      }
      if (opt.equals("helpfile")){
         String fname = optVal.substring(1,optVal.length()-1);
            currentVar.helpfile=fname;
         foundOpt = true;
         return;
      }
        original();
    }

}
