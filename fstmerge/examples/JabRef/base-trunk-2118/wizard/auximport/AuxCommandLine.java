







package net.sf.jabref.wizard.auximport ;

import net.sf.jabref.* ;
import java.util.*;

public class AuxCommandLine
{
  private String auxName ;
  private BibtexDatabase bib ;

  public AuxCommandLine(String auxFileName, BibtexDatabase refDBase)
  {
    auxName = Util.getCorrectFileName(auxFileName, "aux") ;
    bib = refDBase ;
  }

  public BibtexDatabase perform()
  {
    BibtexDatabase back = null ;
    if ( (auxName.length() > 0) && (bib != null) )
    {
      AuxSubGenerator auxParser = new AuxSubGenerator(bib) ;
      Vector returnValue = auxParser.generate(auxName, bib) ;
      back = auxParser.getGeneratedDatabase() ;

      

      System.out.println( Globals.lang("keys_in_database") +" " +bib.getEntryCount() ) ;
      System.out.println( Globals.lang("found_in_aux_file") +" "+auxParser.getFoundKeysInAux());
      System.out.println( Globals.lang("resolved") +" " +auxParser.getResolvedKeysCount());
      if (auxParser.getNotResolvedKeysCount() > 0)
      {
        System.out.println( Globals.lang( "not_found" ) + " " +
                            auxParser.getNotResolvedKeysCount() ) ;
        System.out.println( returnValue ) ;
      }
      int nested = auxParser.getNestedAuxCounter() ;
      if (nested > 0)
        System.out.println( Globals.lang("nested_aux_files") +" " +nested);

    }
    return back ;
  }
}
