









package net.sf.jabref.wizard.auximport ; 

import java.io.BufferedReader; 
import java.io.File; 
import java.io.FileNotFoundException; 
import java.io.FileReader; 
import java.io.IOException; 
import java.util.HashSet; 
import java.util.Iterator; 
import java.util.Vector; 
import java.util.regex.Matcher; 
import java.util.regex.Pattern; 

import net.sf.jabref.BibtexDatabase; 
import net.sf.jabref.BibtexEntry; 
import net.sf.jabref.KeyCollisionException; 
import net.sf.jabref.Util; 

public  class  AuxSubGenerator {
	

  private HashSet<String> mySet ;

	 

  private Vector<String> notFoundList ;

	 

  private BibtexDatabase db ;

	 
  private BibtexDatabase auxDB ;

	 

  private int nestedAuxCounter ;

	  
  private int crossreferencedEntriesCount = 0;

	 

  public AuxSubGenerator(BibtexDatabase refDBase)
  {
    mySet = new HashSet<String>(20) ;
    notFoundList = new Vector<String>() ;
    db = refDBase ;
  }


	

  public final void setReferenceDatabase(BibtexDatabase newRefDB)
  {
    db = newRefDB ;
  }


	

  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  

  public final boolean parseAuxFile(String filename)
  {
    
    Pattern pattern ;
    Matcher matcher ;

    
    boolean weiter = false ;

    
    boolean back = true ;

    
    boolean loopFileOpen = false ;

    
    pattern = Pattern.compile( "\\\\citation\\{.+\\}" ) ;

    
    BufferedReader br = null ;

    
    Vector<String> fileList = new Vector<String>(5) ;
    fileList.add( filename );

    
    File dummy = new File( filename ) ;
    String path = dummy.getParent() ;
    if (path != null)
      path = path + File.separator ;
    else
      path = "" ;

    nestedAuxCounter = -1 ;  

    
    int fileIndex = 0 ;

    while (fileIndex < fileList.size())
    {
      String fName = fileList.elementAt( fileIndex ) ;
      try
      {

        br = new BufferedReader( new FileReader( fName ) ) ;
        weiter = true ;
        loopFileOpen = true ;
      }
      catch ( FileNotFoundException fnfe )
      {
        System.out.println( "Cannot locate input file! " + fnfe.getMessage() ) ;
        
        back = false ;
        weiter = false ;
        loopFileOpen = false ;
      }

      while ( weiter )
      {
        String line ;
        try
        {
            if (br == null)
                throw new IOException();
            line = br.readLine() ;
        }
        catch ( IOException ioe )
        {
          line = null ;
          weiter = false ;
        }

        if ( line != null )
        {
          matcher = pattern.matcher( line ) ;

          while ( matcher.find() )
          {
            
            int len = matcher.end() - matcher.start() ;
            if ( len > 11 )
            {
              String str = matcher.group().substring( matcher.start() + 10,
                  matcher.end() - 1 ) ;
              
              String keys[] = str.split( "," ) ;
              if ( keys != null )
              {
                int keyCount = keys.length ;
                for ( int t = 0 ; t < keyCount ; t++ )
                {
                  String dummyStr = keys[t] ;
                  if ( dummyStr != null )
                  {
                    
                    mySet.add( dummyStr.trim() ) ;

                  }
                }
              }
            }
          }
          
          int index = line.indexOf( "\\@input{" ) ;
          if ( index >= 0 )
          {
            int start = index + 8 ;
            int end = line.indexOf( "}", start ) ;
            if ( end > start )
            {
              String str = path + line.substring( index + 8, end ) ;

              
              if (!fileList.contains( str ) )
              {
                 fileList.add(str);   
              }
            }
          }
        } 
        else weiter = false ;
      } 

      if ( loopFileOpen ) 
      {
        try
        {
            if (br != null)
                br.close() ;
          nestedAuxCounter++ ;
        }
        catch ( IOException ioe )
        {}
      }

      fileIndex++ ; 
    }

    return back ;
  }


	

  
  public final void resolveTags()
  {
    auxDB = new BibtexDatabase() ;
    notFoundList.clear();

    Iterator<String> it = mySet.iterator() ;

    
    
    while (it.hasNext())
    {
      String str = it.next() ;
      BibtexEntry entry = db.getEntryByKey(str);

      if (entry == null)
      {
        notFoundList.add(str) ;
      } else
      {
          insertEntry(auxDB, entry);
          
          
          
          String crossref = entry.getField("crossref");
          if ((crossref != null) && (!mySet.contains(crossref))) {
              BibtexEntry refEntry = db.getEntryByKey(crossref);
              
              if (refEntry == null) {
                  notFoundList.add(crossref);
              } else {
                  insertEntry(auxDB, refEntry);
                  crossreferencedEntriesCount++;
              }
          }

      }
    }
  }


	

    
    private void insertEntry(BibtexDatabase auxDB, BibtexEntry entry) {
        try {
            BibtexEntry clonedEntry = (BibtexEntry)entry.clone();
            clonedEntry.setId(Util.createNeutralId());
            auxDB.insertEntry(clonedEntry);
        } catch (KeyCollisionException e) {
            e.printStackTrace();
        }
    }


	

    
    public final Vector<String> generate(String auxFileName, BibtexDatabase bibDB)
    {
      setReferenceDatabase(bibDB);
      parseAuxFile(auxFileName) ;
      resolveTags();

      return notFoundList ;
    }


	

  public BibtexDatabase getGeneratedDatabase()
  {
    if (auxDB == null)
      auxDB = new BibtexDatabase() ;

    return auxDB ;
  }


	

  public final int getFoundKeysInAux()
  {
    return mySet.size() ;
  }


	

  public final int getResolvedKeysCount()
  {
    return auxDB.getEntryCount() - crossreferencedEntriesCount;
  }


	

  public final int getNotResolvedKeysCount()
  {
    return notFoundList.size() ;
  }


	

    
    public final int getCrossreferencedEntriesCount()
    {
        return crossreferencedEntriesCount;
    }


	

  
  public final void clear()
  {
    mySet.clear() ;
    notFoundList.clear();
    crossreferencedEntriesCount = 0;
    
  }


	

  
  public Vector<String> getNotFoundList()
  {
    return notFoundList ;
  }


	

  
  public int getNestedAuxCounter()
  {
    return this.nestedAuxCounter ;
  }



}
