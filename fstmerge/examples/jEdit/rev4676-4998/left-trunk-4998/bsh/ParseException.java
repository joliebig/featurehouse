



package bsh;


 

public class ParseException extends EvalError {


	

	String sourceFile = "<unknown>";

	
	public void setErrorSourceFile( String file ) {
		this.sourceFile = file;
	}

	public String getErrorSourceFile() { 
		return sourceFile; 
	}

	

  
  public ParseException(Token currentTokenVal,
                        int[][] expectedTokenSequencesVal,
                        String[] tokenImageVal
                       )
  {
	
	this();
	
    specialConstructor = true;
    currentToken = currentTokenVal;
    expectedTokenSequences = expectedTokenSequencesVal;
    tokenImage = tokenImageVal;
  }

  

  public ParseException() {
	
	this("");
	
    specialConstructor = false;
  }

  public ParseException(String message) {
	
	
	super( message, null, null );
	
    specialConstructor = false;
  }

  
  protected boolean specialConstructor;

  
  public Token currentToken;

  
  public int[][] expectedTokenSequences;

  
  public String[] tokenImage;

  
  public String	getMessage() {
	return getMessage( false );
  }
  

  
  
  public String getMessage( boolean debug ) {
  
    if (!specialConstructor) {
      return super.getMessage();
    }
    String expected = "";
    int maxSize = 0;
    for (int i = 0; i < expectedTokenSequences.length; i++) {
      if (maxSize < expectedTokenSequences[i].length) {
        maxSize = expectedTokenSequences[i].length;
      }
      for (int j = 0; j < expectedTokenSequences[i].length; j++) {
        expected += tokenImage[expectedTokenSequences[i][j]] + " ";
      }
      if (expectedTokenSequences[i][expectedTokenSequences[i].length - 1] != 0) {
        expected += "...";
      }
      expected += eol + "    ";
    }
	
    String retval = "In file: "+ sourceFile +" Encountered \"";
	
    Token tok = currentToken.next;
    for (int i = 0; i < maxSize; i++) {
      if (i != 0) retval += " ";
      if (tok.kind == 0) {
        retval += tokenImage[0];
        break;
      }
      retval += add_escapes(tok.image);
      tok = tok.next; 
    }
    retval += "\" at line " + currentToken.next.beginLine + ", column " + currentToken.next.beginColumn + "." + eol;

	
	if ( debug )
	{
		if (expectedTokenSequences.length == 1) {
		  retval += "Was expecting:" + eol + "    ";
		} else {
		  retval += "Was expecting one of:" + eol + "    ";
		}

		retval += expected;
	}
	

    return retval;
  }

  
  protected String eol = System.getProperty("line.separator", "\n");
 
  
  protected String add_escapes(String str) {
      StringBuffer retval = new StringBuffer();
      char ch;
      for (int i = 0; i < str.length(); i++) {
        switch (str.charAt(i))
        {
           case 0 :
              continue;
           case '\b':
              retval.append("\\b");
              continue;
           case '\t':
              retval.append("\\t");
              continue;
           case '\n':
              retval.append("\\n");
              continue;
           case '\f':
              retval.append("\\f");
              continue;
           case '\r':
              retval.append("\\r");
              continue;
           case '\"':
              retval.append("\\\"");
              continue;
           case '\'':
              retval.append("\\\'");
              continue;
           case '\\':
              retval.append("\\\\");
              continue;
           default:
              if ((ch = str.charAt(i)) < 0x20 || ch > 0x7e) {
                 String s = "0000" + Integer.toString(ch, 16);
                 retval.append("\\u" + s.substring(s.length() - 4, s.length()));
              } else {
                 retval.append(ch);
              }
              continue;
        }
      }
      return retval.toString();
   }

	

	public int getErrorLineNumber() 
	{
		return currentToken.next.beginLine;
	}

	public String getErrorText() { 
		
		int	maxSize	= 0;
		for	(int i = 0; i <	expectedTokenSequences.length; i++) {
		  if (maxSize < expectedTokenSequences[i].length)
			maxSize	= expectedTokenSequences[i].length;
		}

		String retval = "";
		Token tok =	currentToken.next;
		for	(int i = 0; i <	maxSize; i++) 
		{
		  if (i != 0) retval += " ";
		  if (tok.kind == 0) {
			retval += tokenImage[0];
			break;
		  }
		  retval +=	add_escapes(tok.image);
		  tok = tok.next;
		}
		
		return retval;
	}

	public String toString() {
		return getMessage();
	}

	

}
