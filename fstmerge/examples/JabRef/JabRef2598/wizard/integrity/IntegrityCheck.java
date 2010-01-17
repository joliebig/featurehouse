













package net.sf.jabref.wizard.integrity ; 

import java.util.Vector; 

import net.sf.jabref.BibtexDatabase; 
import net.sf.jabref.BibtexEntry; 

public  class  IntegrityCheck {
	
  private Vector<IntegrityMessage> messages ;

	

  public IntegrityCheck()
  {
    messages = new Vector<IntegrityMessage>() ;
  }


	

  public Vector<IntegrityMessage> checkBibtexDatabase(BibtexDatabase base) {
		messages.clear();
		if (base != null) {
			for (BibtexEntry entry : base.getEntries()) {
				checkSingleEntry(entry);
			}
		}
		return new Vector<IntegrityMessage>(messages);
	}


	

	public Vector<IntegrityMessage> checkBibtexEntry(BibtexEntry entry) {
		messages.clear();
		checkSingleEntry(entry);
		return new Vector<IntegrityMessage>(messages);
	}


	

  public void checkSingleEntry(BibtexEntry entry)
  {
    if (entry == null)
      return ;

    Object data = entry.getField("author") ;
    if (data != null)
      authorNameCheck( data.toString(), "author", entry) ;

    data = entry.getField("editor") ;
    if (data != null)
      authorNameCheck( data.toString(), "editor", entry) ;

    data = entry.getField("title") ;
    if (data != null)
      titleCheck( data.toString(), "title", entry) ;

    data = entry.getField("year") ;
    if (data != null)
      yearCheck( data.toString(), "year", entry) ;
  }


	

 
  private void authorNameCheck(String names, String fieldName, BibtexEntry entry)
  {
    
    
    StringBuffer structure = new StringBuffer() ;
    int len = names.length() ;
    int mode = -1 ;
    for (int t = 0 ; t < len ; t++)
    {
      char ch = names.charAt(t) ;
      switch (ch)
      {
        case ',' :
          if (mode == 5) 
            structure.append('a') ;
          else
            structure.append('N') ;

          structure.append(',') ;
          mode = 0 ;
          break ;

        case ' ' :
          if (mode == 5) 
            structure.append('a') ;
          else
            if (mode != 0)
              structure.append('N') ;
          mode = -1 ; 
          break ;
       case 'a' :
         if (mode == -1)
           mode = 2 ;
         break ;
       case 'n' :
         if (mode == 2)
           mode = 3 ;
         break ;
       case 'd' :
         if (mode == 3)
           mode = 5 ;
         break ;
       default :
         mode = 1 ;
      }
    }
    if (mode == 5) 
      structure.append('a') ;
    else
      if (mode != 0)
        structure.append('N') ;

    
    len = structure.length() ;
    if (len > 0)
    {
      if (structure.charAt(0) != 'N')  
      {
        messages.add( new IntegrityMessage( IntegrityMessage.NAME_START_WARNING,
                                            entry, fieldName, null))  ;

      }

      if (structure.charAt( structure.length() -1) != 'N')  
      {
        messages.add( new IntegrityMessage( IntegrityMessage.NAME_END_WARNING,
                                            entry, fieldName, null))  ;

      }
      
    }



  }


	



  private void titleCheck(String title, String fieldName, BibtexEntry entry)
  {
    int len = title.length() ;
    int mode = 0 ;
    int upLowCounter = 0 ;

    for (int t = 0 ; t < len ; t++)
    {
      char ch = title.charAt( t ) ;
      switch (ch)
      {
        case '}' : 
          if (mode == 0)
          {
            
            messages.add( new IntegrityMessage( IntegrityMessage.UNEXPECTED_CLOSING_BRACE_FAILURE,
                                            entry, fieldName, null))  ;
          }
          else  
          {
            mode-- ;

          }
          break ;

        case '{' :  
          mode++ ;
          break ;

        case ' ' :

          break ;

        default :
          if (mode == 0) 
          {
            if ( Character.isUpperCase(ch) && (t > 1))
            {
              upLowCounter++ ;
            }
          }
      }
    }
    if (upLowCounter > 0)
    {

        

    }
  }


	

  
  private void yearCheck(String number, String fieldName, BibtexEntry entry)
  {
    int len = number.length() ;
    int digitCounter = 0 ;
    boolean fourDigitsBlock = false ;
    boolean containsFourDigits = false ;

    for (int t = 0 ; t < len ; t++)
    {
      char ch = number.charAt( t ) ;
      if ( Character.isDigit(ch))
      {
        digitCounter++ ;
        if (digitCounter == 4)
          fourDigitsBlock = true ;
        else
          fourDigitsBlock = false ;
      } else
      {
        if (fourDigitsBlock)
          containsFourDigits = true ;

        digitCounter = 0 ;
      }
    }

    if ((!containsFourDigits) && (!fourDigitsBlock))
    {
      messages.add( new IntegrityMessage( IntegrityMessage.FOUR_DIGITS_HINT,
                                      entry, fieldName, null))  ;
    }
  }



}
