

















package net.sf.jabref ;

import java.util.HashMap;
import java.util.Vector;

import net.sf.jabref.util.TXMLReader;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class BibtexFields
{
  public static final String KEY_FIELD = "bibtexkey" ;

  
  public static final String
      SEARCH = "__search",
      GROUPSEARCH = "__groupsearch",
      MARKED = "__markedentry",
      OWNER = "owner",
      TIMESTAMP = "timestamp", 
      ENTRYTYPE = "entrytype",

      
      
      DEFAULT_BIBTEXENTRY_ID = "__ID" ;

  public static final String[] DEFAULT_INSPECTION_FIELDS = new String[]
          {"author", "title", "year", KEY_FIELD};


  
  private static final BibtexFields runtime = new BibtexFields() ;

  
  private HashMap<String, BibtexSingleField> fieldSet;

  
  private String[] PUBLIC_FIELDS = null ;

  private BibtexFields()
  {
    fieldSet = new HashMap<String, BibtexSingleField>();
    BibtexSingleField dummy = null ;

    
    
    
    add( new BibtexSingleField( "address", true, GUIGlobals.SMALL_W  ) ) ;
    
    
    
    add( new BibtexSingleField( "annote", true, GUIGlobals.LARGE_W  ) ) ;
    add( new BibtexSingleField( "author", true, GUIGlobals.MEDIUM_W, 280 ) ) ;
    add( new BibtexSingleField( "booktitle", true, 175 ) ) ;
    add( new BibtexSingleField( "chapter", true, GUIGlobals.SMALL_W  ) ) ;
    add( new BibtexSingleField( "crossref", true, GUIGlobals.SMALL_W ) ) ;
    add( new BibtexSingleField( "edition", true, GUIGlobals.SMALL_W  ) ) ;
    add( new BibtexSingleField( "editor", true, GUIGlobals.MEDIUM_W, 280  ) ) ;
    add( new BibtexSingleField( "howpublished", true, GUIGlobals.MEDIUM_W  ) ) ;
    add( new BibtexSingleField( "institution", true, GUIGlobals.MEDIUM_W  ) ) ;

    dummy = new BibtexSingleField( "journal", true, GUIGlobals.SMALL_W ) ;
    dummy.setExtras("journalNames");
    add(dummy) ;
    add( new BibtexSingleField( "key", true ) ) ;
    add( new BibtexSingleField( "month", true, GUIGlobals.SMALL_W ) ) ;
    add( new BibtexSingleField( "note", true, GUIGlobals.MEDIUM_W  ) ) ;
    add( new BibtexSingleField( "number", true, GUIGlobals.SMALL_W, 60  ).setNumeric(true) ) ;
    add( new BibtexSingleField( "organization", true, GUIGlobals.MEDIUM_W  ) ) ;
    add( new BibtexSingleField( "pages", true, GUIGlobals.SMALL_W ) ) ;
    add( new BibtexSingleField( "publisher", true, GUIGlobals.MEDIUM_W  ) ) ;
    add( new BibtexSingleField( "school", true, GUIGlobals.MEDIUM_W  ) ) ;
    add( new BibtexSingleField( "series", true, GUIGlobals.SMALL_W  ) ) ;
    add( new BibtexSingleField( "title", true, 400 ) ) ;
    add( new BibtexSingleField( "type", true, GUIGlobals.SMALL_W  ) ) ;
    add( new BibtexSingleField( "volume", true, GUIGlobals.SMALL_W, 60  ).setNumeric(true) ) ;
    add( new BibtexSingleField( "year", true, GUIGlobals.SMALL_W, 60 ).setNumeric(true) ) ;

    
    dummy = new BibtexSingleField( KEY_FIELD, true ) ;
    dummy.setPrivate();
    add( dummy ) ;

    dummy = new BibtexSingleField( "doi", true, GUIGlobals.SMALL_W ) ;
    dummy.setExtras("external");
    add(dummy) ;
    add( new BibtexSingleField( "eid", true, GUIGlobals.SMALL_W  ) ) ;

    dummy = new BibtexSingleField( "date", true ) ;
    dummy.setPrivate();
    add( dummy ) ;

    add(new BibtexSingleField("pmid", false, GUIGlobals.SMALL_W, 60).setNumeric(true));

    
    dummy =  new BibtexSingleField( "citeseercitationcount", false,
                                                 GUIGlobals.SMALL_W, 75) ;
    dummy.setNumeric(true);
    dummy.setAlternativeDisplayName("Popularity") ;
    add(dummy) ;
    add( new BibtexSingleField( "location", false ) ) ;
    add( new BibtexSingleField( "abstract", false, GUIGlobals.LARGE_W, 400  ) ) ;

    dummy =  new BibtexSingleField( "url", false, GUIGlobals.SMALL_W) ;
    dummy.setExtras("external");
    add(dummy) ;

    dummy = new BibtexSingleField( "citeseerurl", false, GUIGlobals.SMALL_W ) ;
    dummy.setExtras("external");
    add(dummy) ;

    dummy = new BibtexSingleField( "pdf", false, GUIGlobals.SMALL_W ) ;
    dummy.setExtras("browseDoc");
    add(dummy) ;

    dummy = new BibtexSingleField( "ps", false, GUIGlobals.SMALL_W ) ;
    dummy.setExtras("browseDocZip");
    add(dummy) ;
    add( new BibtexSingleField( "comment", false, GUIGlobals.MEDIUM_W  ) ) ;
    add( new BibtexSingleField( "keywords", false, GUIGlobals.SMALL_W  ) ) ;
    


    dummy = new BibtexSingleField(GUIGlobals.FILE_FIELD, false);
    dummy.setEditorType(GUIGlobals.FILE_LIST_EDITOR);
    add(dummy);


    add( new BibtexSingleField( "search", false, 75 ) ) ;


    
    dummy = new BibtexSingleField( GUIGlobals.NUMBER_COL, false, 32  ) ;
    dummy.setPrivate() ;
    dummy.setWriteable(false);
    dummy.setDisplayable(false);
    add( dummy ) ;

    dummy = new BibtexSingleField( OWNER, false, GUIGlobals.SMALL_W ) ;
    dummy.setExtras("setOwner");
    dummy.setPrivate();
    add(dummy) ;

    dummy = new BibtexSingleField( TIMESTAMP, false, GUIGlobals.SMALL_W ) ;
    dummy.setExtras("datepicker");
    dummy.setPrivate();
    add(dummy) ;

    dummy =  new BibtexSingleField( ENTRYTYPE, false, 75 ) ;
    dummy.setPrivate();
    add(dummy) ;

    dummy =  new BibtexSingleField( SEARCH, false) ;
    dummy.setPrivate();
    dummy.setWriteable(false);
    dummy.setDisplayable(false);
    add(dummy) ;

    dummy =  new BibtexSingleField( GROUPSEARCH, false) ;
    dummy.setPrivate();
    dummy.setWriteable(false);
    dummy.setDisplayable(false);
    add(dummy) ;

    dummy =  new BibtexSingleField( MARKED, false) ;
    dummy.setPrivate();
    dummy.setWriteable(true); 
    dummy.setDisplayable(false);
    add(dummy) ;

     
    readXML( Globals.additionalFields ) ;

    
    Vector<String> pFields = new Vector<String>( fieldSet.size()) ;
    for (BibtexSingleField sField : fieldSet.values()){
      if (sField.isPublic() )
      {
        pFields.add( sField.getFieldName() );
        
        
      }
    }

    PUBLIC_FIELDS = pFields.toArray(new String[pFields.size()]);
    
    java.util.Arrays.sort( PUBLIC_FIELDS );
  }


  
  private void add( BibtexSingleField field )
  {
    
    String key = field.name ;
    fieldSet.put( key, field ) ;
  }

  
  private void readXML( String resName )
  {
    TXMLReader reader = new TXMLReader(resName) ;
    if (reader.isReady() )
    {
      
      NodeList fieldNodes = reader.getNodes("field") ;

      int tagsCount = fieldNodes.getLength() ;
      for (int t = 0 ; t < tagsCount ; t++)
      {
        Element entry = (Element) fieldNodes.item(t) ;
        String fName = reader.readStringAttribute(entry, "name", null) ;
        if (fName != null)  
        {
          fName = fName.toLowerCase() ;
          BibtexSingleField dummy = fieldSet.get( fName ) ;
          if (dummy == null)  
          {
            dummy = new BibtexSingleField(reader, entry) ;
            fieldSet.put(fName, dummy) ;
          }
        }
      }
    }
  }

  
  
  
  private static final BibtexSingleField getField( String name )
  {
    if (name != null)
    {
      return runtime.fieldSet.get(name.toLowerCase()) ;
    }

    return null ;
  }

  public static String getFieldExtras( String name )
  {
    BibtexSingleField sField = getField( name ) ;
    if (sField != null)
    {
      return sField.getExtras() ;
    }
    return null ;
  }


  public static int getEditorType(String name) {
    BibtexSingleField sField = getField( name ) ;
    if (sField != null)
    {
      return sField.getEditorType();
    }
    return GUIGlobals.STANDARD_EDITOR;      
  }

  public static double getFieldWeight( String name )
  {
    BibtexSingleField sField = getField( name ) ;
    if (sField != null)
    {
      return sField.getWeight() ;
    }
    return GUIGlobals.DEFAULT_FIELD_WEIGHT ;
  }

  public static void setFieldWeight( String fieldName, double weight )
  {
    BibtexSingleField sField = getField( fieldName ) ;
    if (sField != null)
    {
      sField.setWeight( weight ) ;
    }
  }

  public static int getFieldLength( String name )
  {
    BibtexSingleField sField = getField( name ) ;
    if (sField != null)
    {
      return sField.getLength() ;
    }
    return GUIGlobals.DEFAULT_FIELD_LENGTH ;
  }

  
  public static String getFieldDisplayName( String fieldName )
  {
    BibtexSingleField sField = getField( fieldName ) ;
    if (sField != null)
    {
      return sField.getAlternativeDisplayName() ;
    }
    return null ;
  }

  public static boolean isWriteableField( String field )
  {
    BibtexSingleField sField = getField( field ) ;
    if (sField != null)
    {
      return sField.isWriteable() ;
    }
    return true ;
  }

  public static boolean isDisplayableField( String field )
  {
    BibtexSingleField sField = getField( field ) ;
    if (sField != null)
    {
      return sField.isDisplayable() ;
    }
    return true ;
  }

  
  public static boolean isStandardField( String field )
  {
    BibtexSingleField sField = getField( field ) ;
    if (sField != null)
    {
      return sField.isStandard() ;
    }
    return false ;
  }

    public static boolean isNumeric( String field ) {
        BibtexSingleField sField = getField( field ) ;
        if (sField != null)
        {
            return sField.isNumeric() ;
        }
        return false ;
    }

  
  public static String[] getAllFieldNames()
  {
    return runtime.PUBLIC_FIELDS ;
  }

  
  public static String getFieldName( int t )
  {
    return  runtime.PUBLIC_FIELDS[t] ;
  }

  
  public static int numberOfPublicFields()
  {
    return runtime.PUBLIC_FIELDS.length ;
  }

  


  
  
  
  private class BibtexSingleField
  {
    private static final int
        STANDARD       = 0x01,  
        PRIVATE        = 0x02,  
        DISPLAYABLE    = 0x04,  
        WRITEABLE      = 0x08 ; 

    
    private String name ;

    
    
    private int flag = DISPLAYABLE | WRITEABLE ;

    private int length = GUIGlobals.DEFAULT_FIELD_LENGTH ;
    private double weight = GUIGlobals.DEFAULT_FIELD_WEIGHT ;

    private int editorType = GUIGlobals.STANDARD_EDITOR;

    
    
    private String alternativeDisplayName = null ;

    
    
    
    private String extras = null ;

    
    
    private boolean numeric = false;

      
    
    

    
    
    

    
    public BibtexSingleField( String fieldName )
    {
      name = fieldName ;
    }

    public BibtexSingleField( String fieldName, boolean pStandard )
    {
      name = fieldName ;
      setFlag( pStandard, STANDARD) ;
    }

    public BibtexSingleField( String fieldName, boolean pStandard, double pWeight)
    {
      name = fieldName ;
      setFlag( pStandard, STANDARD) ;
      weight = pWeight ;
    }

    public BibtexSingleField( String fieldName, boolean pStandard, int pLength)
    {
      name = fieldName ;
      setFlag( pStandard, STANDARD) ;
      length = pLength ;
    }

    public BibtexSingleField( String fieldName, boolean pStandard,
                              double pWeight, int pLength)
    {
      name = fieldName ;
      setFlag( pStandard, STANDARD) ;
      weight = pWeight ;
      length = pLength ;
    }

    
    public BibtexSingleField( TXMLReader reader, Element node)
    {
      
      flag = DISPLAYABLE | WRITEABLE ;

      name = reader.readStringAttribute(node, "name", "field") ;
      name = name.toLowerCase() ;

      
      String wStr = reader.readStringAttribute(node, "weight", null) ;
      if (wStr != null)
      {
        int hCode = wStr.toLowerCase().hashCode() ;
        if (hCode == "small".hashCode())
        {
          weight = GUIGlobals.SMALL_W ;
        }
        else if (hCode == "medium".hashCode())
        {
          weight = GUIGlobals.MEDIUM_W ;
        }
        else if (hCode == "large".hashCode())
        {
          weight = GUIGlobals.LARGE_W ;
        }
        else 
        {
          try
          {
            weight = Double.parseDouble(wStr) ;
            if ((weight < 0.0) || (weight > GUIGlobals.MAX_FIELD_WEIGHT))
            {
              weight = GUIGlobals.DEFAULT_FIELD_WEIGHT ;
            }
          }
          catch (Exception e)
          {
            weight = GUIGlobals.DEFAULT_FIELD_WEIGHT ;
          }
        }
      }
      length = reader.readIntegerAttribute( node, "length", GUIGlobals.DEFAULT_FIELD_LENGTH ) ;

      extras = reader.readStringAttribute(node, "extras", null) ;
    }

    
    



    private void setFlag( boolean onOff, int flagID)
    {
      if (onOff)  
      {
        flag = flag | flagID ;
      }
      else 
      {
        flag = flag & ( 0xff ^ flagID ) ;
      }
    }

    private boolean isSet( int flagID )
    {
      if ( (flag & flagID) == flagID)
        return true ;

      return false ;
    }

    
    public boolean isStandard()
    {
      return isSet( STANDARD ) ;
    }

    public void setPrivate()
    {
      flag = flag | PRIVATE ;
    }

    public boolean isPrivate()
    {
      return isSet( PRIVATE ) ;
    }

    public void setPublic()
    {
      setFlag( false, PRIVATE ) ;
    }

    public boolean isPublic()
    {
      return !isSet( PRIVATE ) ;
    }

    public void setDisplayable(boolean value)
    {
      setFlag( value, DISPLAYABLE ) ;
    }

    public boolean isDisplayable()
    {
      return isSet(DISPLAYABLE) ;
    }


    public void setWriteable(boolean value)
    {
      setFlag( value, WRITEABLE ) ;
    }

    public boolean isWriteable()
    {
      return isSet( WRITEABLE ) ;
    }

    
    public void setAlternativeDisplayName( String aName)
    {
      alternativeDisplayName = aName ;
    }

    public String getAlternativeDisplayName()
    {
      return alternativeDisplayName ;
    }
    

    public void setExtras( String pExtras)
    {
      extras = pExtras ;
    }

    
    
    public String getExtras()
    {
      return extras ;
    }

    public void setEditorType(int type) {
        editorType = type;
    }

    public int getEditorType() {
        return editorType;
    }
    

    public void setWeight( double value )
    {
      this.weight = value ;
    }

    public double getWeight()
    {
      return this.weight ;
    }

    
    public int getLength()
    {
      return this.length ;
    }

    

    public String getFieldName()
    {
      return name ;
    }


      
      public BibtexSingleField setNumeric(boolean numeric) {
          this.numeric = numeric;
          return this;
      }

      public boolean isNumeric() {
          return numeric;
      }

  }
}
