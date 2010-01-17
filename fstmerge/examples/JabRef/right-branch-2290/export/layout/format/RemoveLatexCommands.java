package net.sf.jabref.export.layout.format;

import net.sf.jabref.export.layout.*;
import net.sf.jabref.Globals;

public class RemoveLatexCommands implements LayoutFormatter {

  int i;

  public String format(String field) {

    StringBuffer sb = new StringBuffer("");
    StringBuffer currentCommand = null;
    char c;
    boolean escaped = false, incommand = false;
    for (i=0; i<field.length(); i++) {
      c = field.charAt(i);
      if (escaped && (c == '\\')) {
        sb.append('\\');
        escaped = false;
      }
      else if (c == '\\') {
        escaped = true;
        incommand = true;
        currentCommand = new StringBuffer();
      }
      else if (!incommand && (c=='{' || c=='}')) {
        
      }

      else if (Character.isLetter((char)c) ||
                (Globals.SPECIAL_COMMAND_CHARS.indexOf(""+(char)c) >= 0)) {
         escaped = false;
         if (!incommand)
           sb.append((char)c);
           
         else {
           currentCommand.append( (char) c);
           if ((currentCommand.length() == 1)
               && (Globals.SPECIAL_COMMAND_CHARS.indexOf(currentCommand.toString()) >= 0)) {
             
 
             incommand = false;
             escaped = false;

           }

        }
      }

      else if (Character.isLetter((char)c)) {
        escaped = false;
        if (!incommand)
          sb.append((char)c);
          
        else
          currentCommand.append((char)c);
      }
      else {
        
        if (!incommand || (!Character.isWhitespace(c) && (c != '{')))
          sb.append((char)c);
        else {
          if (c != '{')
            sb.append((char)c);
        }
        incommand = false;
        escaped = false;
      }
    }

    return sb.toString();
  }
}
