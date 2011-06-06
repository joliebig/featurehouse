
package gnu.regexp;
import java.io.Serializable;
import java.util.Enumeration;
import java.util.NoSuchElementException;


public class REMatchEnumeration implements Enumeration, Serializable {
  private static final int YES = 1;
  private static final int MAYBE = 0;
  private static final int NO = -1;
  
  private int more;
  private REMatch match;
  private RE expr;
  private CharIndexed input;
  private int eflags;
    private int index;

  
  REMatchEnumeration(RE expr, CharIndexed input, int index, int eflags) {
    more = MAYBE;
    this.expr = expr;
    this.input = input;
    this.index = index;
    this.eflags = eflags;
  }

  
  public boolean hasMoreElements() {
    return hasMoreMatches(null);
  }

  
  public boolean hasMoreMatches() {
    return hasMoreMatches(null);
  }

  
  public boolean hasMoreMatches(StringBuffer buffer) {
    if (more == MAYBE) {
	match = expr.getMatchImpl(input,index,eflags,buffer);
	if (match != null) {
	    input.move((match.end[0] > 0) ? match.end[0] : 1);
	    
	    index = (match.end[0] > 0) ? match.end[0] + match.offset : index + 1;
	    more = YES;
	} else more = NO;
    }
    return (more == YES);
  }

  
  public Object nextElement() throws NoSuchElementException {
    return nextMatch();
  }

  
  public REMatch nextMatch() throws NoSuchElementException {
    if (hasMoreElements()) {
	more = (input.isValid()) ? MAYBE : NO;
	return match;
    }
    throw new NoSuchElementException();
  }
}

