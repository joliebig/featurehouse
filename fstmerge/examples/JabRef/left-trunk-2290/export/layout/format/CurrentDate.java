
package net.sf.jabref.export.layout.format;

import java.util.Date;
import java.text.SimpleDateFormat;

import net.sf.jabref.export.layout.LayoutFormatter;



public class CurrentDate implements LayoutFormatter
{
    private static final String defaultFormat = "yyyy.MM.dd hh:mm:ss z";
    
     
    public String format(String fieldText)
    {
      String format = defaultFormat;
      if (fieldText != null && !"".equals(fieldText.trim())) {
        format = fieldText;
      }
      return new SimpleDateFormat(format).format(new Date());
    }
}
