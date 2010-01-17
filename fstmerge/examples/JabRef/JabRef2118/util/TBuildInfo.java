










package net.sf.jabref.util; 

import java.io.* ; 

import java.io.BufferedReader; 
import java.io.IOException; 
import java.io.InputStreamReader; 

public  class  TBuildInfo {
	
  private String BUILD_DATE = "" ;

	
  private String BUILD_VERSION = "devel - 1st edition family" ;

	
  private String BUILD_NUMBER = "1" ;

	


  public TBuildInfo(String path)
  {
    readBuildVersionData(path) ;
  }


	


  
  private void readBuildVersionData(String path)
  {
    String buf = null ;
    int sep = 0 ;
    String Key, Value ;
    BufferedReader input = null ;

    try
    {

      input = new BufferedReader(
          new InputStreamReader( getClass().getResourceAsStream( path) ), 100 ) ;
    }
    catch ( Exception e1 )
    {



        return ;
    }

    try
    {
      while ( ( buf = input.readLine() ) != null )
      {
        if ( buf.length() > 0 )
        { 
          if ( buf.charAt( 0 ) != '#' )
          { 
            sep = buf.indexOf( '=' ) ;
            if ( sep > 0 )
            { 
              Key = buf.substring( 0, sep ) ;
              Value = buf.substring( sep + 1 ) ;
              if ( Key.equals( "builddate" ) )
              {
                BUILD_DATE = Value ;
              }
              else if ( Key.equals( "build" ) )
              {
                BUILD_NUMBER = Value ;
              }
              else if ( Key.equals( "version" ) )
              {
                BUILD_VERSION = Value ;
              }

            }
          } 
        }
      } 
    }
    catch ( IOException iex )
    {


    }

    try
    {
      input.close() ;
    }
    catch ( Exception e )
    {


    }
  }


	

  

  public String getBUILD_DATE()
  {
    return BUILD_DATE;
  }


	

  public String getBUILD_VERSION()
  {
    return BUILD_VERSION;
  }


	

  public String getBUILD_NUMBER()
  {
    return BUILD_NUMBER;
  }



}
