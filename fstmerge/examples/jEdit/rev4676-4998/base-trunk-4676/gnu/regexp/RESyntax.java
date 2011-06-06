

package gnu.regexp;
import java.io.Serializable;
import java.util.BitSet;



public final class RESyntax implements Serializable {
    static final String DEFAULT_LINE_SEPARATOR = System.getProperty("line.separator");

    private static final String SYNTAX_IS_FINAL = RE.getLocalizedMessage("syntax.final");

    private BitSet bits;

    
    private boolean isFinal = false;

    private String lineSeparator = DEFAULT_LINE_SEPARATOR;

  

  
  public static final int RE_BACKSLASH_ESCAPE_IN_LISTS =  0;

  
  public static final int RE_BK_PLUS_QM                =  1;

  
  public static final int RE_CHAR_CLASSES              =  2;

  
  public static final int RE_CONTEXT_INDEP_ANCHORS     =  3; 

  
  public static final int RE_CONTEXT_INDEP_OPS         =  4; 

  
  public static final int RE_CONTEXT_INVALID_OPS       =  5; 

  
  public static final int RE_DOT_NEWLINE               =  6;

  
  public static final int RE_DOT_NOT_NULL              =  7;

  
  public static final int RE_INTERVALS                 =  8;

  
  public static final int RE_LIMITED_OPS               =  9;

  
  public static final int RE_NEWLINE_ALT               = 10; 

  
  public static final int RE_NO_BK_BRACES              = 11; 

  
  public static final int RE_NO_BK_PARENS              = 12;

  
  public static final int RE_NO_BK_REFS                = 13;

  
  public static final int RE_NO_BK_VBAR                = 14;

  
  public static final int RE_NO_EMPTY_RANGES           = 15;

  
  public static final int RE_UNMATCHED_RIGHT_PAREN_ORD = 16;

  
  public static final int RE_HAT_LISTS_NOT_NEWLINE     = 17;

  
  public static final int RE_STINGY_OPS                = 18;

  
  public static final int RE_CHAR_CLASS_ESCAPES        = 19;

  
  public static final int RE_PURE_GROUPING             = 20;

  
  public static final int RE_LOOKAHEAD                 = 21;

  
  public static final int RE_STRING_ANCHORS            = 22;

  
  public static final int RE_COMMENTS                  = 23;

  
  public static final int RE_CHAR_CLASS_ESC_IN_LISTS   = 24;

  private static final int BIT_TOTAL                   = 25;

  
  public static final RESyntax RE_SYNTAX_AWK;

  
  public static final RESyntax RE_SYNTAX_ED;

  
  public static final RESyntax RE_SYNTAX_EGREP;

  
  public static final RESyntax RE_SYNTAX_EMACS;

  
  public static final RESyntax RE_SYNTAX_GREP;

  
  public static final RESyntax RE_SYNTAX_POSIX_AWK;

  
  public static final RESyntax RE_SYNTAX_POSIX_BASIC;

  
  public static final RESyntax RE_SYNTAX_POSIX_EGREP;

  
  public static final RESyntax RE_SYNTAX_POSIX_EXTENDED;

  
  public static final RESyntax RE_SYNTAX_POSIX_MINIMAL_BASIC;

  
  public static final RESyntax RE_SYNTAX_POSIX_MINIMAL_EXTENDED;

  
  public static final RESyntax RE_SYNTAX_SED;

  
  public static final RESyntax RE_SYNTAX_PERL4;

  
  public static final RESyntax RE_SYNTAX_PERL4_S; 

  
  public static final RESyntax RE_SYNTAX_PERL5;  

  
  public static final RESyntax RE_SYNTAX_PERL5_S;

    
    public static final RESyntax RE_SYNTAX_JAVA_1_4;

  static {
      
      
      RE_SYNTAX_EMACS = new RESyntax().makeFinal();
      
      RESyntax RE_SYNTAX_POSIX_COMMON = new RESyntax()
	  .set(RE_CHAR_CLASSES)
	  .set(RE_DOT_NEWLINE)
	  .set(RE_DOT_NOT_NULL)
	  .set(RE_INTERVALS)
	  .set(RE_NO_EMPTY_RANGES)
	  .makeFinal();
      
      RE_SYNTAX_POSIX_BASIC = new RESyntax(RE_SYNTAX_POSIX_COMMON)
	  .set(RE_BK_PLUS_QM)
	  .makeFinal();
      
      RE_SYNTAX_POSIX_EXTENDED = new RESyntax(RE_SYNTAX_POSIX_COMMON)
	  .set(RE_CONTEXT_INDEP_ANCHORS)
	  .set(RE_CONTEXT_INDEP_OPS)
	  .set(RE_NO_BK_BRACES)
	  .set(RE_NO_BK_PARENS)
	  .set(RE_NO_BK_VBAR)
	  .set(RE_UNMATCHED_RIGHT_PAREN_ORD)
	  .makeFinal();

      RE_SYNTAX_AWK = new RESyntax()
	  .set(RE_BACKSLASH_ESCAPE_IN_LISTS)
	  .set(RE_DOT_NOT_NULL)
	  .set(RE_NO_BK_PARENS)
	  .set(RE_NO_BK_REFS)
	  .set(RE_NO_BK_VBAR)
	  .set(RE_NO_EMPTY_RANGES)
	  .set(RE_UNMATCHED_RIGHT_PAREN_ORD)
	  .makeFinal();
      
      RE_SYNTAX_POSIX_AWK = new RESyntax(RE_SYNTAX_POSIX_EXTENDED)
	  .set(RE_BACKSLASH_ESCAPE_IN_LISTS)
	  .makeFinal();
      
      RE_SYNTAX_GREP = new RESyntax()
	  .set(RE_BK_PLUS_QM)
	  .set(RE_CHAR_CLASSES)
	  .set(RE_HAT_LISTS_NOT_NEWLINE)
	  .set(RE_INTERVALS)
	  .set(RE_NEWLINE_ALT)
	  .makeFinal();
      
      RE_SYNTAX_EGREP = new RESyntax()
	  .set(RE_CHAR_CLASSES)
	  .set(RE_CONTEXT_INDEP_ANCHORS)
	  .set(RE_CONTEXT_INDEP_OPS)
	  .set(RE_HAT_LISTS_NOT_NEWLINE)
	  .set(RE_NEWLINE_ALT)
	  .set(RE_NO_BK_PARENS)
	  .set(RE_NO_BK_VBAR)
	  .makeFinal();
    
      RE_SYNTAX_POSIX_EGREP = new RESyntax(RE_SYNTAX_EGREP)
	  .set(RE_INTERVALS)
	  .set(RE_NO_BK_BRACES)
	  .makeFinal();
    
      
    
      RE_SYNTAX_ED = new RESyntax(RE_SYNTAX_POSIX_BASIC)
	  .makeFinal();
    
      RE_SYNTAX_SED = new RESyntax(RE_SYNTAX_POSIX_BASIC)
	  .makeFinal();
      
      RE_SYNTAX_POSIX_MINIMAL_BASIC = new RESyntax(RE_SYNTAX_POSIX_COMMON)
	  .set(RE_LIMITED_OPS)
	  .makeFinal();
      
      
      
      RE_SYNTAX_POSIX_MINIMAL_EXTENDED = new RESyntax(RE_SYNTAX_POSIX_COMMON)
	  .set(RE_CONTEXT_INDEP_ANCHORS)
	  .set(RE_CONTEXT_INVALID_OPS)
	  .set(RE_NO_BK_BRACES)
	  .set(RE_NO_BK_PARENS)
	  .set(RE_NO_BK_REFS)
	  .set(RE_NO_BK_VBAR)
	  .set(RE_UNMATCHED_RIGHT_PAREN_ORD)
	  .makeFinal();
      
      
      
      RE_SYNTAX_PERL4 = new RESyntax()
	  .set(RE_BACKSLASH_ESCAPE_IN_LISTS)
	  .set(RE_CONTEXT_INDEP_ANCHORS)
	  .set(RE_CONTEXT_INDEP_OPS)          
	  .set(RE_INTERVALS)
	  .set(RE_NO_BK_BRACES)
	  .set(RE_NO_BK_PARENS)
	  .set(RE_NO_BK_VBAR)
	  .set(RE_NO_EMPTY_RANGES)
	  .set(RE_CHAR_CLASS_ESCAPES)    
	  .makeFinal();
      
      RE_SYNTAX_PERL4_S = new RESyntax(RE_SYNTAX_PERL4)
	  .set(RE_DOT_NEWLINE)
	  .makeFinal();
      
      RE_SYNTAX_PERL5 = new RESyntax(RE_SYNTAX_PERL4)
	  .set(RE_PURE_GROUPING)          
	  .set(RE_STINGY_OPS)             
	  .set(RE_LOOKAHEAD)              
	  .set(RE_STRING_ANCHORS)         
	  .set(RE_CHAR_CLASS_ESC_IN_LISTS)
	  .set(RE_COMMENTS)              
	  .makeFinal();
      
      RE_SYNTAX_PERL5_S = new RESyntax(RE_SYNTAX_PERL5)
	  .set(RE_DOT_NEWLINE)
	  .makeFinal();

      RE_SYNTAX_JAVA_1_4 = new RESyntax(RE_SYNTAX_PERL5)
	  
	  .makeFinal();
  }

  
  public RESyntax() {
    bits = new BitSet(BIT_TOTAL);
  }

    
    public RESyntax makeFinal() {
	isFinal = true;
	return this;
    }

  
  public RESyntax(RESyntax other) {
    bits = (BitSet) other.bits.clone();
  }

  
  public boolean get(int index) {
    return bits.get(index);
  }

  
  public RESyntax set(int index) {
      if (isFinal) throw new IllegalAccessError(SYNTAX_IS_FINAL);
    bits.set(index);
    return this;
  }

  
  public RESyntax clear(int index) {
      if (isFinal) throw new IllegalAccessError(SYNTAX_IS_FINAL);
      bits.clear(index);
      return this;
  }

    
    public RESyntax setLineSeparator(String aSeparator) {
	if (isFinal) throw new IllegalAccessError(SYNTAX_IS_FINAL);
	lineSeparator = aSeparator;
	return this;
    }

    
    public String getLineSeparator() {
	return lineSeparator;
    }
}
