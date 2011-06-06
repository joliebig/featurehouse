

package gnu.regexp;

import java.text.MessageFormat;



public class REException extends Exception {
  private int type;
  private int pos;

  

  
  public static final int REG_BADRPT  =  1;

  
  public static final int REG_BADBR   =  2;

  
  public static final int REG_EBRACE  =  3;

  
  public static final int REG_EBRACK  =  4;

  
  public static final int REG_ERANGE  =  5;

  
  public static final int REG_ECTYPE  =  6;

  
  public static final int REG_EPAREN  =  7;

  
  public static final int REG_ESUBREG =  8;

  
  public static final int REG_EEND    =  9;

  
  public static final int REG_ESCAPE  = 10;

  
  public static final int REG_BADPAT  = 11;

  
  public static final int REG_ESIZE   = 12;

  
  public static final int REG_ESPACE  = 13;

  REException(String msg, int type, int position) { 
    super(msg); 
    this.type = type;
    this.pos = position;
  }

  

  public int getType() {
    return type;
  }

  
  public int getPosition() {
    return pos;
  }

  
  public String getMessage() {
    Object[] args = {new Integer(pos)};
    StringBuffer sb = new StringBuffer();
    String prefix = RE.getLocalizedMessage("error.prefix");
    sb.append(MessageFormat.format(prefix, args));
    sb.append('\n');
    sb.append(super.getMessage());
    return sb.toString();
  }
}
