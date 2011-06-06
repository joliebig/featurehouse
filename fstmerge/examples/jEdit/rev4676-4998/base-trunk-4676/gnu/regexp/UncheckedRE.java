

package gnu.regexp;



public final class UncheckedRE extends RE {
  
  public UncheckedRE(Object pattern) {
      this(pattern,0,RESyntax.RE_SYNTAX_PERL5);
  }

  
  public UncheckedRE(Object pattern, int cflags) {
      this(pattern,cflags,RESyntax.RE_SYNTAX_PERL5);
  }

  
  public UncheckedRE(Object pattern, int cflags, RESyntax syntax) {
      try {
	  initialize(pattern,cflags,syntax,0,0);
      } catch (REException e) { 
	  throw new RuntimeException(e.getMessage());
      }
  }
}


