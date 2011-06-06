package com.lowagie.text.pdf.codec;


public class Base64 {
    
    
    
    
    
    public final static int NO_OPTIONS = 0;
    
    
    public final static int ENCODE = 1;
    
    
    
    public final static int DECODE = 0;
    
    
    
    public final static int GZIP = 2;
    
    
    
    public final static int DONT_BREAK_LINES = 8;
    
    
    public final static int URL_SAFE = 16;
    
    
    
    public final static int ORDERED = 32;
    
    
    
    
    
    
    private final static int MAX_LINE_LENGTH = 76;
    
    
    
    private final static byte EQUALS_SIGN = (byte)'=';
    
    
    
    private final static byte NEW_LINE = (byte)'\n';
    
    
    
    private final static String PREFERRED_ENCODING = "UTF-8";
    
    
    
    
    private final static byte WHITE_SPACE_ENC = -5; 
    private final static byte EQUALS_SIGN_ENC = -1; 
    
    
    
    
    
    
    
    private final static byte[] _STANDARD_ALPHABET =
    {
        (byte)'A', (byte)'B', (byte)'C', (byte)'D', (byte)'E', (byte)'F', (byte)'G',
        (byte)'H', (byte)'I', (byte)'J', (byte)'K', (byte)'L', (byte)'M', (byte)'N',
        (byte)'O', (byte)'P', (byte)'Q', (byte)'R', (byte)'S', (byte)'T', (byte)'U',
        (byte)'V', (byte)'W', (byte)'X', (byte)'Y', (byte)'Z',
        (byte)'a', (byte)'b', (byte)'c', (byte)'d', (byte)'e', (byte)'f', (byte)'g',
        (byte)'h', (byte)'i', (byte)'j', (byte)'k', (byte)'l', (byte)'m', (byte)'n',
        (byte)'o', (byte)'p', (byte)'q', (byte)'r', (byte)'s', (byte)'t', (byte)'u',
        (byte)'v', (byte)'w', (byte)'x', (byte)'y', (byte)'z',
        (byte)'0', (byte)'1', (byte)'2', (byte)'3', (byte)'4', (byte)'5',
        (byte)'6', (byte)'7', (byte)'8', (byte)'9', (byte)'+', (byte)'/'
    };
    
    
    
    private final static byte[] _STANDARD_DECODABET =
    {
        -9,-9,-9,-9,-9,-9,-9,-9,-9,                 
        -5,-5,                                      
        -9,-9,                                      
        -5,                                         
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     
        -9,-9,-9,-9,-9,                             
        -5,                                         
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,              
        62,                                         
        -9,-9,-9,                                   
        63,                                         
        52,53,54,55,56,57,58,59,60,61,              
        -9,-9,-9,                                   
        -1,                                         
        -9,-9,-9,                                      
        0,1,2,3,4,5,6,7,8,9,10,11,12,13,            
        14,15,16,17,18,19,20,21,22,23,24,25,        
        -9,-9,-9,-9,-9,-9,                          
        26,27,28,29,30,31,32,33,34,35,36,37,38,     
        39,40,41,42,43,44,45,46,47,48,49,50,51,     
        -9,-9,-9,-9                                 
        
    };
    
    
    
    
    
    private final static byte[] _URL_SAFE_ALPHABET =
    {
        (byte)'A', (byte)'B', (byte)'C', (byte)'D', (byte)'E', (byte)'F', (byte)'G',
        (byte)'H', (byte)'I', (byte)'J', (byte)'K', (byte)'L', (byte)'M', (byte)'N',
        (byte)'O', (byte)'P', (byte)'Q', (byte)'R', (byte)'S', (byte)'T', (byte)'U',
        (byte)'V', (byte)'W', (byte)'X', (byte)'Y', (byte)'Z',
        (byte)'a', (byte)'b', (byte)'c', (byte)'d', (byte)'e', (byte)'f', (byte)'g',
        (byte)'h', (byte)'i', (byte)'j', (byte)'k', (byte)'l', (byte)'m', (byte)'n',
        (byte)'o', (byte)'p', (byte)'q', (byte)'r', (byte)'s', (byte)'t', (byte)'u',
        (byte)'v', (byte)'w', (byte)'x', (byte)'y', (byte)'z',
        (byte)'0', (byte)'1', (byte)'2', (byte)'3', (byte)'4', (byte)'5',
        (byte)'6', (byte)'7', (byte)'8', (byte)'9', (byte)'-', (byte)'_'
    };
    
    
    private final static byte[] _URL_SAFE_DECODABET =
    {
        -9,-9,-9,-9,-9,-9,-9,-9,-9,                 
        -5,-5,                                      
        -9,-9,                                      
        -5,                                         
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     
        -9,-9,-9,-9,-9,                             
        -5,                                         
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,              
        -9,                                         
        -9,                                         
        62,                                         
        -9,                                         
        -9,                                         
        52,53,54,55,56,57,58,59,60,61,              
        -9,-9,-9,                                   
        -1,                                         
        -9,-9,-9,                                   
        0,1,2,3,4,5,6,7,8,9,10,11,12,13,            
        14,15,16,17,18,19,20,21,22,23,24,25,        
        -9,-9,-9,-9,                                
        63,                                         
        -9,                                         
        26,27,28,29,30,31,32,33,34,35,36,37,38,     
        39,40,41,42,43,44,45,46,47,48,49,50,51,     
        -9,-9,-9,-9                                 
      
    };
    
    
    
    
    
    
    private final static byte[] _ORDERED_ALPHABET =
    {
        (byte)'-',
        (byte)'0', (byte)'1', (byte)'2', (byte)'3', (byte)'4',
        (byte)'5', (byte)'6', (byte)'7', (byte)'8', (byte)'9',
        (byte)'A', (byte)'B', (byte)'C', (byte)'D', (byte)'E', (byte)'F', (byte)'G',
        (byte)'H', (byte)'I', (byte)'J', (byte)'K', (byte)'L', (byte)'M', (byte)'N',
        (byte)'O', (byte)'P', (byte)'Q', (byte)'R', (byte)'S', (byte)'T', (byte)'U',
        (byte)'V', (byte)'W', (byte)'X', (byte)'Y', (byte)'Z',
        (byte)'_',
        (byte)'a', (byte)'b', (byte)'c', (byte)'d', (byte)'e', (byte)'f', (byte)'g',
        (byte)'h', (byte)'i', (byte)'j', (byte)'k', (byte)'l', (byte)'m', (byte)'n',
        (byte)'o', (byte)'p', (byte)'q', (byte)'r', (byte)'s', (byte)'t', (byte)'u',
        (byte)'v', (byte)'w', (byte)'x', (byte)'y', (byte)'z'
    };
    
    
    private final static byte[] _ORDERED_DECODABET =
    {
        -9,-9,-9,-9,-9,-9,-9,-9,-9,                 
        -5,-5,                                      
        -9,-9,                                      
        -5,                                         
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,     
        -9,-9,-9,-9,-9,                             
        -5,                                         
        -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,              
        -9,                                         
        -9,                                         
        0,                                          
        -9,                                         
        -9,                                         
        1,2,3,4,5,6,7,8,9,10,                       
        -9,-9,-9,                                   
        -1,                                         
        -9,-9,-9,                                   
        11,12,13,14,15,16,17,18,19,20,21,22,23,     
        24,25,26,27,28,29,30,31,32,33,34,35,36,     
        -9,-9,-9,-9,                                
        37,                                         
        -9,                                         
        38,39,40,41,42,43,44,45,46,47,48,49,50,     
        51,52,53,54,55,56,57,58,59,60,61,62,63,     
        -9,-9,-9,-9                                 
      
    };
    
    
    
    
    
    
    private final static byte[] getAlphabet( int options ) {
        if( (options & URL_SAFE) == URL_SAFE ) return _URL_SAFE_ALPHABET;
        else if( (options & ORDERED) == ORDERED ) return _ORDERED_ALPHABET;
        else return _STANDARD_ALPHABET;
        
    }    
    
    
    
    private final static byte[] getDecodabet( int options ) {
        if( (options & URL_SAFE) == URL_SAFE ) return _URL_SAFE_DECODABET;
        else if( (options & ORDERED) == ORDERED ) return _ORDERED_DECODABET;
        else return _STANDARD_DECODABET;
        
    }    
    
    
    
    
    private Base64(){}
    
    
    
    public final static void main( String[] args ) {
        if( args.length < 3 ){
            usage("Not enough arguments.");
        }   
        else {
            String flag = args[0];
            String infile = args[1];
            String outfile = args[2];
            if( flag.equals( "-e" ) ){
                Base64.encodeFileToFile( infile, outfile );
            }   
            else if( flag.equals( "-d" ) ) {
                Base64.decodeFileToFile( infile, outfile );
            }   
            else {
                usage( "Unknown flag: " + flag );
            }   
        }   
    }   
    
    
    private final static void usage( String msg ) {
        System.err.println( msg );
        System.err.println( "Usage: java Base64 -e|-d inputfile outputfile" );
    }   
    
    
    
    
    
    
    private static byte[] encode3to4( byte[] b4, byte[] threeBytes, int numSigBytes, int options ) {
        encode3to4( threeBytes, 0, numSigBytes, b4, 0, options );
        return b4;
    }   
    
    
    
    private static byte[] encode3to4(
            byte[] source, int srcOffset, int numSigBytes,
            byte[] destination, int destOffset, int options ) {
        byte[] ALPHABET = getAlphabet( options );
        
        
        
        
        
        
        
        
        
        
        
        
        int inBuff =   ( numSigBytes > 0 ? ((source[ srcOffset     ] << 24) >>>  8) : 0 )
        | ( numSigBytes > 1 ? ((source[ srcOffset + 1 ] << 24) >>> 16) : 0 )
        | ( numSigBytes > 2 ? ((source[ srcOffset + 2 ] << 24) >>> 24) : 0 );
        
        switch( numSigBytes ) {
            case 3:
                destination[ destOffset     ] = ALPHABET[ (inBuff >>> 18)        ];
                destination[ destOffset + 1 ] = ALPHABET[ (inBuff >>> 12) & 0x3f ];
                destination[ destOffset + 2 ] = ALPHABET[ (inBuff >>>  6) & 0x3f ];
                destination[ destOffset + 3 ] = ALPHABET[ (inBuff       ) & 0x3f ];
                return destination;
                
            case 2:
                destination[ destOffset     ] = ALPHABET[ (inBuff >>> 18)        ];
                destination[ destOffset + 1 ] = ALPHABET[ (inBuff >>> 12) & 0x3f ];
                destination[ destOffset + 2 ] = ALPHABET[ (inBuff >>>  6) & 0x3f ];
                destination[ destOffset + 3 ] = EQUALS_SIGN;
                return destination;
                
            case 1:
                destination[ destOffset     ] = ALPHABET[ (inBuff >>> 18)        ];
                destination[ destOffset + 1 ] = ALPHABET[ (inBuff >>> 12) & 0x3f ];
                destination[ destOffset + 2 ] = EQUALS_SIGN;
                destination[ destOffset + 3 ] = EQUALS_SIGN;
                return destination;
                
            default:
                return destination;
        }   
    }   
    
    
    
    
    public static String encodeObject( java.io.Serializable serializableObject ) {
        return encodeObject( serializableObject, NO_OPTIONS );
    }   
    
    
    
    
    public static String encodeObject( java.io.Serializable serializableObject, int options ) {
        
        java.io.ByteArrayOutputStream  baos  = null;
        java.io.OutputStream           b64os = null;
        java.io.ObjectOutputStream     oos   = null;
        java.util.zip.GZIPOutputStream gzos  = null;
        
        
        int gzip           = (options & GZIP);
        int dontBreakLines = (options & DONT_BREAK_LINES);
        
        try {
            
            baos  = new java.io.ByteArrayOutputStream();
            b64os = new Base64.OutputStream( baos, ENCODE | options );
            
            
            if( gzip == GZIP ) {
                gzos = new java.util.zip.GZIPOutputStream( b64os );
                oos  = new java.io.ObjectOutputStream( gzos );
            }   
            else
                oos   = new java.io.ObjectOutputStream( b64os );
            
            oos.writeObject( serializableObject );
        }   
        catch( java.io.IOException e ) {
            e.printStackTrace();
            return null;
        }   
        finally {
            try{ oos.close();   } catch( Exception e ){}
            try{ gzos.close();  } catch( Exception e ){}
            try{ b64os.close(); } catch( Exception e ){}
            try{ baos.close();  } catch( Exception e ){}
        }   
        
        
        try {
            return new String( baos.toByteArray(), PREFERRED_ENCODING );
        }   
        catch (java.io.UnsupportedEncodingException uue) {
            return new String( baos.toByteArray() );
        }   
        
    }   
    
    
    
    
    public static String encodeBytes( byte[] source ) {
        return encodeBytes( source, 0, source.length, NO_OPTIONS );
    }   
    
    
    
    
    public static String encodeBytes( byte[] source, int options ) {
        return encodeBytes( source, 0, source.length, options );
    }   
    
    
    
    public static String encodeBytes( byte[] source, int off, int len ) {
        return encodeBytes( source, off, len, NO_OPTIONS );
    }   
    
    
    
    
    public static String encodeBytes( byte[] source, int off, int len, int options ) {
        
        int dontBreakLines = ( options & DONT_BREAK_LINES );
        int gzip           = ( options & GZIP   );
        
        
        if( gzip == GZIP ) {
            java.io.ByteArrayOutputStream  baos  = null;
            java.util.zip.GZIPOutputStream gzos  = null;
            Base64.OutputStream            b64os = null;
            
            
            try {
                
                baos = new java.io.ByteArrayOutputStream();
                b64os = new Base64.OutputStream( baos, ENCODE | options );
                gzos  = new java.util.zip.GZIPOutputStream( b64os );
                
                gzos.write( source, off, len );
                gzos.close();
            }   
            catch( java.io.IOException e ) {
                e.printStackTrace();
                return null;
            }   
            finally {
                try{ gzos.close();  } catch( Exception e ){}
                try{ b64os.close(); } catch( Exception e ){}
                try{ baos.close();  } catch( Exception e ){}
            }   
            
            
            try {
                return new String( baos.toByteArray(), PREFERRED_ENCODING );
            }   
            catch (java.io.UnsupportedEncodingException uue) {
                return new String( baos.toByteArray() );
            }   
        }   
        
        
        else {
            
            boolean breakLines = dontBreakLines == 0;
            
            int    len43   = len * 4 / 3;
            byte[] outBuff = new byte[   ( len43 )                      
            + ( (len % 3) > 0 ? 4 : 0 )      
            + (breakLines ? ( len43 / MAX_LINE_LENGTH ) : 0) ]; 
            int d = 0;
            int e = 0;
            int len2 = len - 2;
            int lineLength = 0;
            for( ; d < len2; d+=3, e+=4 ) {
                encode3to4( source, d+off, 3, outBuff, e, options );
                
                lineLength += 4;
                if( breakLines && lineLength == MAX_LINE_LENGTH ) {
                    outBuff[e+4] = NEW_LINE;
                    e++;
                    lineLength = 0;
                }   
            }   
            
            if( d < len ) {
                encode3to4( source, d+off, len - d, outBuff, e, options );
                e += 4;
            }   
            
            
            
            try {
                return new String( outBuff, 0, e, PREFERRED_ENCODING );
            }   
            catch (java.io.UnsupportedEncodingException uue) {
                return new String( outBuff, 0, e );
            }   
            
        }   
        
    }   
    
    
    
    
    
    
    
    
    
    private static int decode4to3( byte[] source, int srcOffset, byte[] destination, int destOffset, int options ) {
        byte[] DECODABET = getDecodabet( options );
        
        
        if( source[ srcOffset + 2] == EQUALS_SIGN ) {
            
            
            
            int outBuff =   ( ( DECODABET[ source[ srcOffset    ] ] & 0xFF ) << 18 )
            | ( ( DECODABET[ source[ srcOffset + 1] ] & 0xFF ) << 12 );
            
            destination[ destOffset ] = (byte)( outBuff >>> 16 );
            return 1;
        }
        
        
        else if( source[ srcOffset + 3 ] == EQUALS_SIGN ) {
            
            
            
            
            int outBuff =   ( ( DECODABET[ source[ srcOffset     ] ] & 0xFF ) << 18 )
            | ( ( DECODABET[ source[ srcOffset + 1 ] ] & 0xFF ) << 12 )
            | ( ( DECODABET[ source[ srcOffset + 2 ] ] & 0xFF ) <<  6 );
            
            destination[ destOffset     ] = (byte)( outBuff >>> 16 );
            destination[ destOffset + 1 ] = (byte)( outBuff >>>  8 );
            return 2;
        }
        
        
        else {
            try{
                
                
                
                
                
                int outBuff =   ( ( DECODABET[ source[ srcOffset     ] ] & 0xFF ) << 18 )
                | ( ( DECODABET[ source[ srcOffset + 1 ] ] & 0xFF ) << 12 )
                | ( ( DECODABET[ source[ srcOffset + 2 ] ] & 0xFF ) <<  6)
                | ( ( DECODABET[ source[ srcOffset + 3 ] ] & 0xFF )      );
                
                
                destination[ destOffset     ] = (byte)( outBuff >> 16 );
                destination[ destOffset + 1 ] = (byte)( outBuff >>  8 );
                destination[ destOffset + 2 ] = (byte)( outBuff       );
                
                return 3;
            }catch( Exception e){
                System.out.println(""+source[srcOffset]+ ": " + ( DECODABET[ source[ srcOffset     ] ]  ) );
                System.out.println(""+source[srcOffset+1]+  ": " + ( DECODABET[ source[ srcOffset + 1 ] ]  ) );
                System.out.println(""+source[srcOffset+2]+  ": " + ( DECODABET[ source[ srcOffset + 2 ] ]  ) );
                System.out.println(""+source[srcOffset+3]+  ": " + ( DECODABET[ source[ srcOffset + 3 ] ]  ) );
                return -1;
            }   
        }
    }   
    
    
    
    
    
    public static byte[] decode( byte[] source, int off, int len, int options ) {
        byte[] DECODABET = getDecodabet( options );
        
        int    len34   = len * 3 / 4;
        byte[] outBuff = new byte[ len34 ]; 
        int    outBuffPosn = 0;
        
        byte[] b4        = new byte[4];
        int    b4Posn    = 0;
        int    i         = 0;
        byte   sbiCrop   = 0;
        byte   sbiDecode = 0;
        for( i = off; i < off+len; i++ ) {
            sbiCrop = (byte)(source[i] & 0x7f); 
            sbiDecode = DECODABET[ sbiCrop ];
            
            if( sbiDecode >= WHITE_SPACE_ENC ) 
            {
                if( sbiDecode >= EQUALS_SIGN_ENC ) {
                    b4[ b4Posn++ ] = sbiCrop;
                    if( b4Posn > 3 ) {
                        outBuffPosn += decode4to3( b4, 0, outBuff, outBuffPosn, options );
                        b4Posn = 0;
                        
                        
                        if( sbiCrop == EQUALS_SIGN )
                            break;
                    }   
                    
                }   
                
            }   
            else {
                System.err.println( "Bad Base64 input character at " + i + ": " + source[i] + "(decimal)" );
                return null;
            }   
        }   
        
        byte[] out = new byte[ outBuffPosn ];
        System.arraycopy( outBuff, 0, out, 0, outBuffPosn );
        return out;
    }   
    
    
    
    
    
    public static byte[] decode( String s ) {
        return decode( s, NO_OPTIONS );
    }
    
    
    
    public static byte[] decode( String s, int options ) {
        byte[] bytes;
        try {
            bytes = s.getBytes( PREFERRED_ENCODING );
        }   
        catch( java.io.UnsupportedEncodingException uee ) {
            bytes = s.getBytes();
        }   
        
        
        
        bytes = decode( bytes, 0, bytes.length, options );
        
        
        
        
        if( bytes != null && bytes.length >= 4 ) {
            
            int head = (bytes[0] & 0xff) | ((bytes[1] << 8) & 0xff00);
            if( java.util.zip.GZIPInputStream.GZIP_MAGIC == head ) {
                java.io.ByteArrayInputStream  bais = null;
                java.util.zip.GZIPInputStream gzis = null;
                java.io.ByteArrayOutputStream baos = null;
                byte[] buffer = new byte[2048];
                int    length = 0;
                
                try {
                    baos = new java.io.ByteArrayOutputStream();
                    bais = new java.io.ByteArrayInputStream( bytes );
                    gzis = new java.util.zip.GZIPInputStream( bais );
                    
                    while( ( length = gzis.read( buffer ) ) >= 0 ) {
                        baos.write(buffer,0,length);
                    }   
                    
                    
                    bytes = baos.toByteArray();
                    
                }   
                catch( java.io.IOException e ) {
                    
                }   
                finally {
                    try{ baos.close(); } catch( Exception e ){}
                    try{ gzis.close(); } catch( Exception e ){}
                    try{ bais.close(); } catch( Exception e ){}
                }   
                
            }   
        }   
        
        return bytes;
    }   
    
    
    
    
    
    public static Object decodeToObject( String encodedObject ) {
        
        byte[] objBytes = decode( encodedObject );
        
        java.io.ByteArrayInputStream  bais = null;
        java.io.ObjectInputStream     ois  = null;
        Object obj = null;
        
        try {
            bais = new java.io.ByteArrayInputStream( objBytes );
            ois  = new java.io.ObjectInputStream( bais );
            
            obj = ois.readObject();
        }   
        catch( java.io.IOException e ) {
            e.printStackTrace();
            obj = null;
        }   
        catch( java.lang.ClassNotFoundException e ) {
            e.printStackTrace();
            obj = null;
        }   
        finally {
            try{ bais.close(); } catch( Exception e ){}
            try{ ois.close();  } catch( Exception e ){}
        }   
        
        return obj;
    }   
    
    
    
    
    public static boolean encodeToFile( byte[] dataToEncode, String filename ) {
        boolean success = false;
        Base64.OutputStream bos = null;
        try {
            bos = new Base64.OutputStream(
                    new java.io.FileOutputStream( filename ), Base64.ENCODE );
            bos.write( dataToEncode );
            success = true;
        }   
        catch( java.io.IOException e ) {
            
            success = false;
        }   
        finally {
            try{ bos.close(); } catch( Exception e ){}
        }   
        
        return success;
    }   
    
    
    
    public static boolean decodeToFile( String dataToDecode, String filename ) {
        boolean success = false;
        Base64.OutputStream bos = null;
        try {
            bos = new Base64.OutputStream(
                    new java.io.FileOutputStream( filename ), Base64.DECODE );
            bos.write( dataToDecode.getBytes( PREFERRED_ENCODING ) );
            success = true;
        }   
        catch( java.io.IOException e ) {
            success = false;
        }   
        finally {
            try{ bos.close(); } catch( Exception e ){}
        }   
        
        return success;
    }   
    
    
    
    
    
    public static byte[] decodeFromFile( String filename ) {
        byte[] decodedData = null;
        Base64.InputStream bis = null;
        try {
            
            java.io.File file = new java.io.File( filename );
            byte[] buffer = null;
            int length   = 0;
            int numBytes = 0;
            
            
            if( file.length() > Integer.MAX_VALUE ) {
                System.err.println( "File is too big for this convenience method (" + file.length() + " bytes)." );
                return null;
            }   
            buffer = new byte[ (int)file.length() ];
            
            
            bis = new Base64.InputStream(
                    new java.io.BufferedInputStream(
                    new java.io.FileInputStream( file ) ), Base64.DECODE );
            
            
            while( ( numBytes = bis.read( buffer, length, 4096 ) ) >= 0 )
                length += numBytes;
            
            
            decodedData = new byte[ length ];
            System.arraycopy( buffer, 0, decodedData, 0, length );
            
        }   
        catch( java.io.IOException e ) {
            System.err.println( "Error decoding from file " + filename );
        }   
        finally {
            try{ bis.close(); } catch( Exception e) {}
        }   
        
        return decodedData;
    }   
    
    
    
    
    public static String encodeFromFile( String filename ) {
        String encodedData = null;
        Base64.InputStream bis = null;
        try {
            
            java.io.File file = new java.io.File( filename );
            byte[] buffer = new byte[ Math.max((int)(file.length() * 1.4),40) ]; 
            int length   = 0;
            int numBytes = 0;
            
            
            bis = new Base64.InputStream(
                    new java.io.BufferedInputStream(
                    new java.io.FileInputStream( file ) ), Base64.ENCODE );
            
            
            while( ( numBytes = bis.read( buffer, length, 4096 ) ) >= 0 )
                length += numBytes;
            
            
            encodedData = new String( buffer, 0, length, Base64.PREFERRED_ENCODING );
            
        }   
        catch( java.io.IOException e ) {
            System.err.println( "Error encoding from file " + filename );
        }   
        finally {
            try{ bis.close(); } catch( Exception e) {}
        }   
        
        return encodedData;
    }   
    
    
    public static void encodeFileToFile( String infile, String outfile ) {
        String encoded = Base64.encodeFromFile( infile );
        java.io.OutputStream out = null;
        try{
            out = new java.io.BufferedOutputStream(
                    new java.io.FileOutputStream( outfile ) );
            out.write( encoded.getBytes("US-ASCII") ); 
        }   
        catch( java.io.IOException ex ) {
            ex.printStackTrace();
        }   
        finally {
            try { out.close(); } catch( Exception ex ){}
        }   
    }   
    
    
    
    public static void decodeFileToFile( String infile, String outfile ) {
        byte[] decoded = Base64.decodeFromFile( infile );
        java.io.OutputStream out = null;
        try{
            out = new java.io.BufferedOutputStream(
                    new java.io.FileOutputStream( outfile ) );
            out.write( decoded );
        }   
        catch( java.io.IOException ex ) {
            ex.printStackTrace();
        }   
        finally {
            try { out.close(); } catch( Exception ex ){}
        }   
    }   
    
    
    
    
    
    
    
    public static class InputStream extends java.io.FilterInputStream {
        private boolean encode;         
        private int     position;       
        private byte[]  buffer;         
        private int     bufferLength;   
        private int     numSigBytes;    
        private int     lineLength;
        private boolean breakLines;     
        private int     options;        
        private byte[]  alphabet;        
        private byte[]  decodabet;        
        
        
        
        public InputStream( java.io.InputStream in ) {
            this( in, DECODE );
        }   
        
        
        
        public InputStream( java.io.InputStream in, int options ) {
            super( in );
            this.breakLines   = (options & DONT_BREAK_LINES) != DONT_BREAK_LINES;
            this.encode       = (options & ENCODE) == ENCODE;
            this.bufferLength = encode ? 4 : 3;
            this.buffer       = new byte[ bufferLength ];
            this.position     = -1;
            this.lineLength   = 0;
            this.options      = options; 
            this.alphabet     = getAlphabet(options);
            this.decodabet    = getDecodabet(options);
        }   
        
        
        public int read() throws java.io.IOException {
            
            if( position < 0 ) {
                if( encode ) {
                    byte[] b3 = new byte[3];
                    int numBinaryBytes = 0;
                    for( int i = 0; i < 3; i++ ) {
                        try {
                            int b = in.read();
                            
                            
                            if( b >= 0 ) {
                                b3[i] = (byte)b;
                                numBinaryBytes++;
                            }   
                            
                        }   
                        catch( java.io.IOException e ) {
                            
                            if( i == 0 )
                                throw e;
                            
                        }   
                    }   
                    
                    if( numBinaryBytes > 0 ) {
                        encode3to4( b3, 0, numBinaryBytes, buffer, 0, options );
                        position = 0;
                        numSigBytes = 4;
                    }   
                    else {
                        return -1;
                    }   
                }   
                
                
                else {
                    byte[] b4 = new byte[4];
                    int i = 0;
                    for( i = 0; i < 4; i++ ) {
                        
                        int b = 0;
                        do{ b = in.read(); }
                        while( b >= 0 && decodabet[ b & 0x7f ] <= WHITE_SPACE_ENC );
                        
                        if( b < 0 )
                            break; 
                        
                        b4[i] = (byte)b;
                    }   
                    
                    if( i == 4 ) {
                        numSigBytes = decode4to3( b4, 0, buffer, 0, options );
                        position = 0;
                    }   
                    else if( i == 0 ){
                        return -1;
                    }   
                    else {
                        
                        throw new java.io.IOException( "Improperly padded Base64 input." );
                    }   
                    
                }   
            }   
            
            
            if( position >= 0 ) {
                
                if(  position >= numSigBytes )
                    return -1;
                
                if( encode && breakLines && lineLength >= MAX_LINE_LENGTH ) {
                    lineLength = 0;
                    return '\n';
                }   
                else {
                    lineLength++;   
                    
                    
                    
                    int b = buffer[ position++ ];
                    
                    if( position >= bufferLength )
                        position = -1;
                    
                    return b & 0xFF; 
                    
                }   
            }   
            
            
            else {
                
                throw new java.io.IOException( "Error in Base64 code reading stream." );
            }   
        }   
        
        
        
        public int read( byte[] dest, int off, int len ) throws java.io.IOException {
            int i;
            int b;
            for( i = 0; i < len; i++ ) {
                b = read();
                
                
                
                
                if( b >= 0 )
                    dest[off + i] = (byte)b;
                else if( i == 0 )
                    return -1;
                else
                    break; 
            }   
            return i;
        }   
        
    }   
    
    
    
    
    
    
    
    
    
    
    
    public static class OutputStream extends java.io.FilterOutputStream {
        private boolean encode;
        private int     position;
        private byte[]  buffer;
        private int     bufferLength;
        private int     lineLength;
        private boolean breakLines;
        private byte[]  b4; 
        private boolean suspendEncoding;
        private int options; 
        private byte[]  alphabet;        
        private byte[]  decodabet;        
        
        
        public OutputStream( java.io.OutputStream out ) {
            this( out, ENCODE );
        }   
        
        
        
        public OutputStream( java.io.OutputStream out, int options ) {
            super( out );
            this.breakLines   = (options & DONT_BREAK_LINES) != DONT_BREAK_LINES;
            this.encode       = (options & ENCODE) == ENCODE;
            this.bufferLength = encode ? 3 : 4;
            this.buffer       = new byte[ bufferLength ];
            this.position     = 0;
            this.lineLength   = 0;
            this.suspendEncoding = false;
            this.b4           = new byte[4];
            this.options      = options;
            this.alphabet     = getAlphabet(options);
            this.decodabet    = getDecodabet(options);
        }   
        
        
        
        public void write(int theByte) throws java.io.IOException {
            
            if( suspendEncoding ) {
                super.out.write( theByte );
                return;
            }   
            
            
            if( encode ) {
                buffer[ position++ ] = (byte)theByte;
                if( position >= bufferLength )  
                {
                    out.write( encode3to4( b4, buffer, bufferLength, options ) );
                    
                    lineLength += 4;
                    if( breakLines && lineLength >= MAX_LINE_LENGTH ) {
                        out.write( NEW_LINE );
                        lineLength = 0;
                    }   
                    
                    position = 0;
                }   
            }   
            
            
            else {
                
                if( decodabet[ theByte & 0x7f ] > WHITE_SPACE_ENC ) {
                    buffer[ position++ ] = (byte)theByte;
                    if( position >= bufferLength )  
                    {
                        int len = Base64.decode4to3( buffer, 0, b4, 0, options );
                        out.write( b4, 0, len );
                        
                        position = 0;
                    }   
                }   
                else if( decodabet[ theByte & 0x7f ] != WHITE_SPACE_ENC ) {
                    throw new java.io.IOException( "Invalid character in Base64 data." );
                }   
            }   
        }   
        
        
        
        
        public void write( byte[] theBytes, int off, int len ) throws java.io.IOException {
            
            if( suspendEncoding ) {
                super.out.write( theBytes, off, len );
                return;
            }   
            
            for( int i = 0; i < len; i++ ) {
                write( theBytes[ off + i ] );
            }   
            
        }   
        
        
        
        
        public void flushBase64() throws java.io.IOException {
            if( position > 0 ) {
                if( encode ) {
                    out.write( encode3to4( b4, buffer, position, options ) );
                    position = 0;
                }   
                else {
                    throw new java.io.IOException( "Base64 input not properly padded." );
                }   
            }   
            
        }   
        
        
        
        public void close() throws java.io.IOException {
            
            flushBase64();
            
            
            
            super.close();
            
            buffer = null;
            out    = null;
        }   
        
        
        
        
        public void suspendEncoding() throws java.io.IOException {
            flushBase64();
            this.suspendEncoding = true;
        }   
        
        
        
        public void resumeEncoding() {
            this.suspendEncoding = false;
        }   
        
        
        
    }   
    
    
}   
