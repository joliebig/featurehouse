

package gnu.regexp;
import java.io.InputStream;
import java.io.Reader;
import java.io.Serializable;
import java.util.Locale;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;
import java.util.Vector;

class IntPair implements Serializable {
  public int first, second;
}

class CharUnit implements Serializable {
  public char ch;
  public boolean bk;
}



public class RE extends REToken {
  
  private static final String VERSION = "1.1.5-dev";

  
  private static ResourceBundle messages = PropertyResourceBundle.getBundle("gnu/regexp/MessagesBundle", Locale.getDefault());

  
  
  private REToken firstToken, lastToken;

  
  
  private int numSubs;

    
    private int minimumLength;

  
  public static final int REG_ICASE = 2;

  
  public static final int REG_DOT_NEWLINE = 4;

  
  public static final int REG_MULTILINE = 8;

  
  public static final int REG_NOTBOL = 16;

  
  public static final int REG_NOTEOL = 32;

  
  public static final int REG_ANCHORINDEX = 64;

  
  public static final int REG_NO_INTERPOLATE = 128;

  
  public static final String version() {
    return VERSION;
  }

  
  static final String getLocalizedMessage(String key) {
    return messages.getString(key);
  }

  
  public RE(Object pattern) throws REException {
    this(pattern,0,RESyntax.RE_SYNTAX_PERL5,0,0);
  }

  
  public RE(Object pattern, int cflags) throws REException {
    this(pattern,cflags,RESyntax.RE_SYNTAX_PERL5,0,0);
  }

  
  public RE(Object pattern, int cflags, RESyntax syntax) throws REException {
    this(pattern,cflags,syntax,0,0);
  }

  
  private RE(REToken first, REToken last,int subs, int subIndex, int minLength) {
    super(subIndex);
    firstToken = first;
    lastToken = last;
    numSubs = subs;
    minimumLength = minLength;
    addToken(new RETokenEndSub(subIndex));
  }

  private RE(Object patternObj, int cflags, RESyntax syntax, int myIndex, int nextSub) throws REException {
    super(myIndex); 
    initialize(patternObj, cflags, syntax, myIndex, nextSub);
  }

    
    protected RE() { super(0); }

    
  protected void initialize(Object patternObj, int cflags, RESyntax syntax, int myIndex, int nextSub) throws REException {
      char[] pattern;
    if (patternObj instanceof String) {
      pattern = ((String) patternObj).toCharArray();
    } else if (patternObj instanceof char[]) {
      pattern = (char[]) patternObj;
    } else if (patternObj instanceof StringBuffer) {
      pattern = new char [((StringBuffer) patternObj).length()];
      ((StringBuffer) patternObj).getChars(0,pattern.length,pattern,0);
    } else {
	pattern = patternObj.toString().toCharArray();
    }

    int pLength = pattern.length;

    numSubs = 0; 
    Vector branches = null;

    
    firstToken = lastToken = null;

    
    
    boolean insens = ((cflags & REG_ICASE) > 0);

    
    

    
    int index = 0;

    
    CharUnit unit = new CharUnit();

    
    IntPair minMax = new IntPair();

    
    REToken currentToken = null;
    char ch;

    while (index < pLength) {
      
      index = getCharUnit(pattern,index,unit);

      
      
      

      
      
      if ( ( (unit.ch == '|' && (syntax.get(RESyntax.RE_NO_BK_VBAR) ^ unit.bk))
	     || (syntax.get(RESyntax.RE_NEWLINE_ALT) && (unit.ch == '\n') && !unit.bk) )
	   && !syntax.get(RESyntax.RE_LIMITED_OPS)) {
	
	addToken(currentToken);
	RE theBranch = new RE(firstToken, lastToken, numSubs, subIndex, minimumLength);
	minimumLength = 0;
	if (branches == null) {
	    branches = new Vector();
	}
	branches.addElement(theBranch);
	firstToken = lastToken = currentToken = null;
      }
      
      
      
      
      
      
      

      else if ((unit.ch == '{') && syntax.get(RESyntax.RE_INTERVALS) && (syntax.get(RESyntax.RE_NO_BK_BRACES) ^ unit.bk)) {
	int newIndex = getMinMax(pattern,index,minMax,syntax);
        if (newIndex > index) {
          if (minMax.first > minMax.second)
            throw new REException(getLocalizedMessage("interval.order"),REException.REG_BADRPT,newIndex);
          if (currentToken == null)
            throw new REException(getLocalizedMessage("repeat.no.token"),REException.REG_BADRPT,newIndex);
          if (currentToken instanceof RETokenRepeated) 
            throw new REException(getLocalizedMessage("repeat.chained"),REException.REG_BADRPT,newIndex);
          if (currentToken instanceof RETokenWordBoundary || currentToken instanceof RETokenWordBoundary)
            throw new REException(getLocalizedMessage("repeat.assertion"),REException.REG_BADRPT,newIndex);
          if ((currentToken.getMinimumLength() == 0) && (minMax.second == Integer.MAX_VALUE))
            throw new REException(getLocalizedMessage("repeat.empty.token"),REException.REG_BADRPT,newIndex);
          index = newIndex;
          currentToken = setRepeated(currentToken,minMax.first,minMax.second,index); 
        }
        else {
          addToken(currentToken);
          currentToken = new RETokenChar(subIndex,unit.ch,insens);
        } 
      }
      
      
      

      else if ((unit.ch == '[') && !unit.bk) {
	Vector options = new Vector();
	boolean negative = false;
	char lastChar = 0;
	if (index == pLength) throw new REException(getLocalizedMessage("unmatched.bracket"),REException.REG_EBRACK,index);
	
	
	if ((ch = pattern[index]) == '^') {
	  negative = true;
	  if (++index == pLength) throw new REException(getLocalizedMessage("class.no.end"),REException.REG_EBRACK,index);
	  ch = pattern[index];
	}

	
	if (ch == ']') {
	  lastChar = ch;
	  if (++index == pLength) throw new REException(getLocalizedMessage("class.no.end"),REException.REG_EBRACK,index);
	}

	while ((ch = pattern[index++]) != ']') {
	  if ((ch == '-') && (lastChar != 0)) {
	    if (index == pLength) throw new REException(getLocalizedMessage("class.no.end"),REException.REG_EBRACK,index);
	    if ((ch = pattern[index]) == ']') {
	      options.addElement(new RETokenChar(subIndex,lastChar,insens));
	      lastChar = '-';
	    } else {
	      options.addElement(new RETokenRange(subIndex,lastChar,ch,insens));
	      lastChar = 0;
	      index++;
	    }
          } else if ((ch == '\\') && syntax.get(RESyntax.RE_BACKSLASH_ESCAPE_IN_LISTS)) {
            if (index == pLength) throw new REException(getLocalizedMessage("class.no.end"),REException.REG_EBRACK,index);
	    int posixID = -1;
	    boolean negate = false;
            char asciiEsc = 0;
	    if (("dswDSW".indexOf(pattern[index]) != -1) && syntax.get(RESyntax.RE_CHAR_CLASS_ESC_IN_LISTS)) {
	      switch (pattern[index]) {
	      case 'D':
		negate = true;
	      case 'd':
		posixID = RETokenPOSIX.DIGIT;
		break;
	      case 'S':
		negate = true;
	      case 's':
		posixID = RETokenPOSIX.SPACE;
		break;
	      case 'W':
		negate = true;
	      case 'w':
		posixID = RETokenPOSIX.ALNUM;
		break;
	      }
	    }
            else if ("nrt".indexOf(pattern[index]) != -1) {
              switch (pattern[index]) {
                case 'n':
                  asciiEsc = '\n';
                  break;
                case 't':
                  asciiEsc = '\t';
                  break;
                case 'r':
                  asciiEsc = '\r';
                  break;
              }
            }
	    if (lastChar != 0) options.addElement(new RETokenChar(subIndex,lastChar,insens));
	    
	    if (posixID != -1) {
	      options.addElement(new RETokenPOSIX(subIndex,posixID,insens,negate));
	    } else if (asciiEsc != 0) {
	      lastChar = asciiEsc;
	    } else {
	      lastChar = pattern[index];
	    }
	    ++index;
	  } else if ((ch == '[') && (syntax.get(RESyntax.RE_CHAR_CLASSES)) && (index < pLength) && (pattern[index] == ':')) {
	    StringBuffer posixSet = new StringBuffer();
	    index = getPosixSet(pattern,index+1,posixSet);
	    int posixId = RETokenPOSIX.intValue(posixSet.toString());
	    if (posixId != -1)
	      options.addElement(new RETokenPOSIX(subIndex,posixId,insens,false));
	  } else {
	    if (lastChar != 0) options.addElement(new RETokenChar(subIndex,lastChar,insens));
	    lastChar = ch;
	  }
	  if (index == pLength) throw new REException(getLocalizedMessage("class.no.end"),REException.REG_EBRACK,index);
	} 
	
	    
	if (lastChar != 0) options.addElement(new RETokenChar(subIndex,lastChar,insens));
	    
	
	addToken(currentToken);
	options.trimToSize();
	currentToken = new RETokenOneOf(subIndex,options,negative);
      }

      
      

      else if ((unit.ch == '(') && (syntax.get(RESyntax.RE_NO_BK_PARENS) ^ unit.bk)) {
	boolean pure = false;
	boolean comment = false;
        boolean lookAhead = false;
        boolean negativelh = false;
	if ((index+1 < pLength) && (pattern[index] == '?')) {
	  switch (pattern[index+1]) {
          case '!':
            if (syntax.get(RESyntax.RE_LOOKAHEAD)) {
              pure = true;
              negativelh = true;
              lookAhead = true;
              index += 2;
            }
            break;
          case '=':
            if (syntax.get(RESyntax.RE_LOOKAHEAD)) {
              pure = true;
              lookAhead = true;
              index += 2;
            }
            break;
	  case ':':
	    if (syntax.get(RESyntax.RE_PURE_GROUPING)) {
	      pure = true;
	      index += 2;
	    }
	    break;
	  case '#':
	    if (syntax.get(RESyntax.RE_COMMENTS)) {
	      comment = true;
	    }
	    break;
          default:
            throw new REException(getLocalizedMessage("repeat.no.token"), REException.REG_BADRPT, index);
	  }
	}

	if (index >= pLength) {
	    throw new REException(getLocalizedMessage("unmatched.paren"), REException.REG_ESUBREG,index);
	}

	
	int endIndex = index;
	int nextIndex = index;
	int nested = 0;

	while ( ((nextIndex = getCharUnit(pattern,endIndex,unit)) > 0)
		&& !(nested == 0 && (unit.ch == ')') && (syntax.get(RESyntax.RE_NO_BK_PARENS) ^ unit.bk)) )
	  if ((endIndex = nextIndex) >= pLength)
	    throw new REException(getLocalizedMessage("subexpr.no.end"),REException.REG_ESUBREG,nextIndex);
	  else if (unit.ch == '(' && (syntax.get(RESyntax.RE_NO_BK_PARENS) ^ unit.bk))
	    nested++;
	  else if (unit.ch == ')' && (syntax.get(RESyntax.RE_NO_BK_PARENS) ^ unit.bk))
	    nested--;

	
	

	if (comment) index = nextIndex;
	else { 
	  
	  addToken(currentToken);
	  if (!pure) {
	    numSubs++;
	  }

	  int useIndex = (pure || lookAhead) ? 0 : nextSub + numSubs;
	  currentToken = new RE(String.valueOf(pattern,index,endIndex-index).toCharArray(),cflags,syntax,useIndex,nextSub + numSubs);
	  numSubs += ((RE) currentToken).getNumSubs();

          if (lookAhead) {
	      currentToken = new RETokenLookAhead(currentToken,negativelh);
	  }

	  index = nextIndex;
	} 
      } 
    
      
      
      
      else if (!syntax.get(RESyntax.RE_UNMATCHED_RIGHT_PAREN_ORD) && ((unit.ch == ')') && (syntax.get(RESyntax.RE_NO_BK_PARENS) ^ unit.bk))) {
	throw new REException(getLocalizedMessage("unmatched.paren"),REException.REG_EPAREN,index);
      }

      
      

      else if ((unit.ch == '^') && !unit.bk) {
	addToken(currentToken);
	currentToken = null;
	addToken(new RETokenStart(subIndex,((cflags & REG_MULTILINE) > 0) ? syntax.getLineSeparator() : null));
      }

      
      

      else if ((unit.ch == '$') && !unit.bk) {
	addToken(currentToken);
	currentToken = null;
	addToken(new RETokenEnd(subIndex,((cflags & REG_MULTILINE) > 0) ? syntax.getLineSeparator() : null));
      }

      
      

      else if ((unit.ch == '.') && !unit.bk) {
	addToken(currentToken);
	currentToken = new RETokenAny(subIndex,syntax.get(RESyntax.RE_DOT_NEWLINE) || ((cflags & REG_DOT_NEWLINE) > 0),syntax.get(RESyntax.RE_DOT_NOT_NULL));
      }

      
      

      else if ((unit.ch == '*') && !unit.bk) {
	if (currentToken == null)
          throw new REException(getLocalizedMessage("repeat.no.token"),REException.REG_BADRPT,index);
	if (currentToken instanceof RETokenRepeated)
          throw new REException(getLocalizedMessage("repeat.chained"),REException.REG_BADRPT,index);
	if (currentToken instanceof RETokenWordBoundary || currentToken instanceof RETokenWordBoundary)
	  throw new REException(getLocalizedMessage("repeat.assertion"),REException.REG_BADRPT,index);
	if (currentToken.getMinimumLength() == 0)
	  throw new REException(getLocalizedMessage("repeat.empty.token"),REException.REG_BADRPT,index);
	currentToken = setRepeated(currentToken,0,Integer.MAX_VALUE,index);
      }

      
      
      

      else if ((unit.ch == '+') && !syntax.get(RESyntax.RE_LIMITED_OPS) && (!syntax.get(RESyntax.RE_BK_PLUS_QM) ^ unit.bk)) {
	if (currentToken == null)
          throw new REException(getLocalizedMessage("repeat.no.token"),REException.REG_BADRPT,index);
	if (currentToken instanceof RETokenRepeated)
          throw new REException(getLocalizedMessage("repeat.chained"),REException.REG_BADRPT,index);
	if (currentToken instanceof RETokenWordBoundary || currentToken instanceof RETokenWordBoundary)
	  throw new REException(getLocalizedMessage("repeat.assertion"),REException.REG_BADRPT,index);
	if (currentToken.getMinimumLength() == 0)
	  throw new REException(getLocalizedMessage("repeat.empty.token"),REException.REG_BADRPT,index);
	currentToken = setRepeated(currentToken,1,Integer.MAX_VALUE,index);
      }

      
      
      
      

      else if ((unit.ch == '?') && !syntax.get(RESyntax.RE_LIMITED_OPS) && (!syntax.get(RESyntax.RE_BK_PLUS_QM) ^ unit.bk)) {
	if (currentToken == null) throw new REException(getLocalizedMessage("repeat.no.token"),REException.REG_BADRPT,index);

	
	if (currentToken instanceof RETokenRepeated) {
          if (syntax.get(RESyntax.RE_STINGY_OPS) && !((RETokenRepeated)currentToken).isStingy())
            ((RETokenRepeated)currentToken).makeStingy();
          else
            throw new REException(getLocalizedMessage("repeat.chained"),REException.REG_BADRPT,index);
        }
        else if (currentToken instanceof RETokenWordBoundary || currentToken instanceof RETokenWordBoundary)
          throw new REException(getLocalizedMessage("repeat.assertion"),REException.REG_BADRPT,index);
	else
	  currentToken = setRepeated(currentToken,0,1,index);
      }
	
      
      
      

      else if (unit.bk && Character.isDigit(unit.ch) && !syntax.get(RESyntax.RE_NO_BK_REFS)) {
	addToken(currentToken);
	currentToken = new RETokenBackRef(subIndex,Character.digit(unit.ch,10),insens);
      }

      
      
      
      else if (unit.bk && (unit.ch == 'A') && syntax.get(RESyntax.RE_STRING_ANCHORS)) {
	addToken(currentToken);
	currentToken = new RETokenStart(subIndex,null);
      }

      
      

      else if (unit.bk && (unit.ch == 'b') && syntax.get(RESyntax.RE_STRING_ANCHORS)) {
	  addToken(currentToken);
	  currentToken = new RETokenWordBoundary(subIndex, RETokenWordBoundary.BEGIN | RETokenWordBoundary.END, false);
      } 

      
      
      else if (unit.bk && (unit.ch == '<')) {
	  addToken(currentToken);
	  currentToken = new RETokenWordBoundary(subIndex, RETokenWordBoundary.BEGIN, false);
      } 

      
      
      else if (unit.bk && (unit.ch == '>')) {
	  addToken(currentToken);
	  currentToken = new RETokenWordBoundary(subIndex, RETokenWordBoundary.END, false);
      } 

      
      

      else if (unit.bk && (unit.ch == 'B') && syntax.get(RESyntax.RE_STRING_ANCHORS)) {
	  addToken(currentToken);
	  currentToken = new RETokenWordBoundary(subIndex, RETokenWordBoundary.BEGIN | RETokenWordBoundary.END, true);
      } 

      
      
      
      
      else if (unit.bk && (unit.ch == 'd') && syntax.get(RESyntax.RE_CHAR_CLASS_ESCAPES)) {
	addToken(currentToken);
	currentToken = new RETokenPOSIX(subIndex,RETokenPOSIX.DIGIT,insens,false);
      }

      
      

	else if (unit.bk && (unit.ch == 'D') && syntax.get(RESyntax.RE_CHAR_CLASS_ESCAPES)) {
	  addToken(currentToken);
	  currentToken = new RETokenPOSIX(subIndex,RETokenPOSIX.DIGIT,insens,true);
	}

	
        

	else if (unit.bk && (unit.ch == 'n')) {
	  addToken(currentToken);
	  currentToken = new RETokenChar(subIndex,'\n',false);
	}

	
        

	else if (unit.bk && (unit.ch == 'r')) {
	  addToken(currentToken);
	  currentToken = new RETokenChar(subIndex,'\r',false);
	}

	
        

	else if (unit.bk && (unit.ch == 's') && syntax.get(RESyntax.RE_CHAR_CLASS_ESCAPES)) {
	  addToken(currentToken);
	  currentToken = new RETokenPOSIX(subIndex,RETokenPOSIX.SPACE,insens,false);
	}

	
        

	else if (unit.bk && (unit.ch == 'S') && syntax.get(RESyntax.RE_CHAR_CLASS_ESCAPES)) {
	  addToken(currentToken);
	  currentToken = new RETokenPOSIX(subIndex,RETokenPOSIX.SPACE,insens,true);
	}

	
        

	else if (unit.bk && (unit.ch == 't')) {
	  addToken(currentToken);
	  currentToken = new RETokenChar(subIndex,'\t',false);
	}

	
        

	else if (unit.bk && (unit.ch == 'w') && syntax.get(RESyntax.RE_CHAR_CLASS_ESCAPES)) {
	  addToken(currentToken);
	  currentToken = new RETokenPOSIX(subIndex,RETokenPOSIX.ALNUM,insens,false);
	}

	
        

	else if (unit.bk && (unit.ch == 'W') && syntax.get(RESyntax.RE_CHAR_CLASS_ESCAPES)) {
	  addToken(currentToken);
	  currentToken = new RETokenPOSIX(subIndex,RETokenPOSIX.ALNUM,insens,true);
	}

	
        

	else if (unit.bk && (unit.ch == 'Z') && syntax.get(RESyntax.RE_STRING_ANCHORS)) {
	  addToken(currentToken);
	  currentToken = new RETokenEnd(subIndex,null);
	}

	
        

	else {  
	  addToken(currentToken);
	  currentToken = new RETokenChar(subIndex,unit.ch,insens);
	} 
      } 

    
    addToken(currentToken);
      
    if (branches != null) {
	branches.addElement(new RE(firstToken,lastToken,numSubs,subIndex,minimumLength));
	branches.trimToSize(); 
	minimumLength = 0;
	firstToken = lastToken = null;
	addToken(new RETokenOneOf(subIndex,branches,false));
    } 
    else addToken(new RETokenEndSub(subIndex));

  }

  private static int getCharUnit(char[] input, int index, CharUnit unit) throws REException {
    unit.ch = input[index++];
    if (unit.bk = (unit.ch == '\\'))
      if (index < input.length)
	unit.ch = input[index++];
      else throw new REException(getLocalizedMessage("ends.with.backslash"),REException.REG_ESCAPE,index);
    return index;
  }

  
  public boolean isMatch(Object input) {
    return isMatch(input,0,0);
  }
  
  
  public boolean isMatch(Object input,int index) {
    return isMatch(input,index,0);
  }
  

  
  public boolean isMatch(Object input,int index,int eflags) {
    return isMatchImpl(makeCharIndexed(input,index),index,eflags);
  }

  private boolean isMatchImpl(CharIndexed input, int index, int eflags) {
    if (firstToken == null)  
      return (input.charAt(0) == CharIndexed.OUT_OF_BOUNDS);
    REMatch m = new REMatch(numSubs, index, eflags);
    if (firstToken.match(input, m)) {
	while (m != null) {
	    if (input.charAt(m.index) == CharIndexed.OUT_OF_BOUNDS) {
		return true;
	    }
	    m = m.next;
	}
    }
    return false;
  }
    
  
  public int getNumSubs() {
    return numSubs;
  }

  
  void setUncle(REToken uncle) {
      if (lastToken != null) {
	  lastToken.setUncle(uncle);
      } else super.setUncle(uncle); 
  }

  

  boolean chain(REToken next) {
    super.chain(next);
    setUncle(next);
    return true;
  }

  
  public int getMinimumLength() {
      return minimumLength;
  }

  
  public REMatch[] getAllMatches(Object input) {
    return getAllMatches(input,0,0);
  }

  
  public REMatch[] getAllMatches(Object input, int index) {
    return getAllMatches(input,index,0);
  }

  
  public REMatch[] getAllMatches(Object input, int index, int eflags) {
    return getAllMatchesImpl(makeCharIndexed(input,index),index,eflags);
  }

  
  private REMatch[] getAllMatchesImpl(CharIndexed input, int index, int eflags) {
    Vector all = new Vector();
    REMatch m = null;
    while ((m = getMatchImpl(input,index,eflags,null)) != null) {
      all.addElement(m);
      index = m.getEndIndex();
      if (m.end[0] == 0) {   
	index++;
	input.move(1);
      } else {
	input.move(m.end[0]);
      }
      if (!input.isValid()) break;
    }
    REMatch[] mset = new REMatch[all.size()];
    all.copyInto(mset);
    return mset;
  }
  
    
    boolean match(CharIndexed input, REMatch mymatch) { 
	if (firstToken == null) return next(input, mymatch);

	
	mymatch.start[subIndex] = mymatch.index;

	return firstToken.match(input, mymatch);
    }
  
  
  public REMatch getMatch(Object input) {
    return getMatch(input,0,0);
  }
  
  
  public REMatch getMatch(Object input, int index) {
    return getMatch(input,index,0);
  }
  
  
  public REMatch getMatch(Object input, int index, int eflags) {
    return getMatch(input,index,eflags,null);
  }

  
  public REMatch getMatch(Object input, int index, int eflags, StringBuffer buffer) {
    return getMatchImpl(makeCharIndexed(input,index),index,eflags,buffer);
  }

  REMatch getMatchImpl(CharIndexed input, int anchor, int eflags, StringBuffer buffer) {
      
      REMatch mymatch = new REMatch(numSubs, anchor, eflags);
      do {
	  
	  if (minimumLength == 0 || input.charAt(minimumLength-1) != CharIndexed.OUT_OF_BOUNDS) {
	      if (match(input, mymatch)) {
		  
		  REMatch longest = mymatch;
		  while ((mymatch = mymatch.next) != null) {
		      if (mymatch.index > longest.index) {
			  longest = mymatch;
		      }
		  }
		  
		  longest.end[0] = longest.index;
		  longest.finish(input);
		  return longest;
	      }
	  }
	  mymatch.clear(++anchor);
	  
	  if (buffer != null && input.charAt(0) != CharIndexed.OUT_OF_BOUNDS) {
	      buffer.append(input.charAt(0));
	  }
      } while (input.move(1));
      
      
      if (minimumLength == 0) {
	  if (match(input, mymatch)) {
	      mymatch.finish(input);
	      return mymatch;
	  }
      }

      return null;
  }

  
  public REMatchEnumeration getMatchEnumeration(Object input) {
    return getMatchEnumeration(input,0,0);
  }


  
  public REMatchEnumeration getMatchEnumeration(Object input, int index) {
    return getMatchEnumeration(input,index,0);
  }

  
  public REMatchEnumeration getMatchEnumeration(Object input, int index, int eflags) {
    return new REMatchEnumeration(this,makeCharIndexed(input,index),index,eflags);
  }


  
  public String substitute(Object input,String replace) {
    return substitute(input,replace,0,0);
  }

  
  public String substitute(Object input,String replace,int index) {
    return substitute(input,replace,index,0);
  }

  
  public String substitute(Object input,String replace,int index,int eflags) {
    return substituteImpl(makeCharIndexed(input,index),replace,index,eflags);
  }

  private String substituteImpl(CharIndexed input,String replace,int index,int eflags) {
    StringBuffer buffer = new StringBuffer();
    REMatch m = getMatchImpl(input,index,eflags,buffer);
    if (m==null) return buffer.toString();
    buffer.append( ((eflags & REG_NO_INTERPOLATE) > 0) ?
		   replace : m.substituteInto(replace) );
    if (input.move(m.end[0])) {
      do {
	buffer.append(input.charAt(0));
      } while (input.move(1));
    }
    return buffer.toString();
  }
  
  
  public String substituteAll(Object input,String replace) {
    return substituteAll(input,replace,0,0);
  }

  
  public String substituteAll(Object input,String replace,int index) {
    return substituteAll(input,replace,index,0);
  }
 
  
  public String substituteAll(Object input,String replace,int index,int eflags) {
    return substituteAllImpl(makeCharIndexed(input,index),replace,index,eflags);
  }

  private String substituteAllImpl(CharIndexed input,String replace,int index,int eflags) {
    StringBuffer buffer = new StringBuffer();
    REMatch m;
    while ((m = getMatchImpl(input,index,eflags,buffer)) != null) {
	buffer.append( ((eflags & REG_NO_INTERPOLATE) > 0) ?
		       replace : m.substituteInto(replace) );
      index = m.getEndIndex();
      if (m.end[0] == 0) {
	char ch = input.charAt(0);
	if (ch != CharIndexed.OUT_OF_BOUNDS) 
	    buffer.append(ch);
	input.move(1);
      } else {
	  input.move(m.end[0]);
      }

      if (!input.isValid()) break;
    }
    return buffer.toString();
  }
  
  
  private void addToken(REToken next) {
    if (next == null) return;
    minimumLength += next.getMinimumLength();
    if (firstToken == null) {
	lastToken = firstToken = next;
    } else {
      
      
      if (lastToken.chain(next)) {
	  lastToken = next;
      }
    }
  }

  private static REToken setRepeated(REToken current, int min, int max, int index) throws REException {
    if (current == null) throw new REException(getLocalizedMessage("repeat.no.token"),REException.REG_BADRPT,index);
    return new RETokenRepeated(current.subIndex,current,min,max);
  }

  private static int getPosixSet(char[] pattern,int index,StringBuffer buf) {
    
    
    int i;
    for (i=index; i<(pattern.length-1); i++) {
      if ((pattern[i] == ':') && (pattern[i+1] == ']'))
	return i+2;
      buf.append(pattern[i]);
    }
    return index; 
  }

  private int getMinMax(char[] input,int index,IntPair minMax,RESyntax syntax) throws REException {
    

    boolean mustMatch = !syntax.get(RESyntax.RE_NO_BK_BRACES);
    int startIndex = index;
    if (index == input.length) {
      if (mustMatch)
        throw new REException(getLocalizedMessage("unmatched.brace"),REException.REG_EBRACE,index);
      else
        return startIndex;
    }
    
    int min,max=0;
    CharUnit unit = new CharUnit();
    StringBuffer buf = new StringBuffer();
    
    
    do {
      index = getCharUnit(input,index,unit);
      if (Character.isDigit(unit.ch))
        buf.append(unit.ch);
    } while ((index != input.length) && Character.isDigit(unit.ch));

    
    if (buf.length() == 0) {
      if (mustMatch)
        throw new REException(getLocalizedMessage("interval.error"),REException.REG_EBRACE,index);
      else
        return startIndex;
    }

    min = Integer.parseInt(buf.toString());
	
    if ((unit.ch == '}') && (syntax.get(RESyntax.RE_NO_BK_BRACES) ^ unit.bk))
      max = min;
    else if (index == input.length)
      if (mustMatch)
        throw new REException(getLocalizedMessage("interval.no.end"),REException.REG_EBRACE,index);
      else
        return startIndex;
    else if ((unit.ch == ',') && !unit.bk) {
      buf = new StringBuffer();
      
      while (((index = getCharUnit(input,index,unit)) != input.length) && Character.isDigit(unit.ch))
	buf.append(unit.ch);

      if (!((unit.ch == '}') && (syntax.get(RESyntax.RE_NO_BK_BRACES) ^ unit.bk)))
        if (mustMatch)
          throw new REException(getLocalizedMessage("interval.error"),REException.REG_EBRACE,index);
        else
          return startIndex;

      
      if (buf.length() == 0) max = Integer.MAX_VALUE;
      else max = Integer.parseInt(buf.toString());
    } else
      if (mustMatch)
        throw new REException(getLocalizedMessage("interval.error"),REException.REG_EBRACE,index);
      else
        return startIndex;

    

    minMax.first = min;
    minMax.second = max;

    
    return index;
  }

   
   public String toString() {
     StringBuffer sb = new StringBuffer();
     dump(sb);
     return sb.toString();
   }

  void dump(StringBuffer os) {
    os.append('(');
    if (subIndex == 0)
      os.append("?:");
    if (firstToken != null)
      firstToken.dumpAll(os);
    os.append(')');
  }

  
  private static CharIndexed makeCharIndexed(Object input, int index) {
      
      
    if (input instanceof String)
      return new CharIndexedString((String) input,index);
    else if (input instanceof char[])
      return new CharIndexedCharArray((char[]) input,index);
    else if (input instanceof StringBuffer)
      return new CharIndexedStringBuffer((StringBuffer) input,index);
    else if (input instanceof InputStream)
      return new CharIndexedInputStream((InputStream) input,index);
    else if (input instanceof Reader)
	return new CharIndexedReader((Reader) input, index);
    else if (input instanceof CharIndexed)
	return (CharIndexed) input; 
    else 
	return new CharIndexedString(input.toString(), index);
  }
}
