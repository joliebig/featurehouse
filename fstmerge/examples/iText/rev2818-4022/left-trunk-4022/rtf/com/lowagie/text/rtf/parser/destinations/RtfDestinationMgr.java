
package com.lowagie.text.rtf.parser.destinations;

import java.util.HashMap;

import com.lowagie.text.rtf.parser.RtfParser;


public final class RtfDestinationMgr {
    
    private static RtfDestinationMgr instance = null;
    private static Object lock = new Object();
    
    
    private static HashMap destinations = new HashMap(300, 0.95f);
    
    private static HashMap destinationObjects = new HashMap(10, 0.95f);
    
    private static boolean ignoreUnknownDestinations = false;
    
    private static RtfParser rtfParser = null;

    
    public static final String DESTINATION_NULL = "null";
    
    public static final String DESTINATION_DOCUMENT = "document";
    
    
    private RtfDestinationMgr() {
    }
    
    public static void setParser(RtfParser parser) {
        rtfParser = parser;
    }
    public static RtfDestinationMgr getInstance() {
        synchronized(lock) {
            if(instance == null) {
                instance = new RtfDestinationMgr();
                
                RtfDestinationMgr.addDestination(RtfDestinationMgr.DESTINATION_DOCUMENT, new Object[] { "RtfDestinationDocument", "" } );
                RtfDestinationMgr.addDestination(RtfDestinationMgr.DESTINATION_NULL, new Object[] { "RtfDestinationNull", "" } );
            }
            return instance;
        }
    }
    public static RtfDestinationMgr getInstance(RtfParser parser) {
        synchronized(lock) {
            RtfDestinationMgr.setParser(parser);
            if(instance == null) {
                instance = new RtfDestinationMgr();
                
                RtfDestinationMgr.addDestination(RtfDestinationMgr.DESTINATION_DOCUMENT, new Object[] { "RtfDestinationDocument", "" } );
                RtfDestinationMgr.addDestination(RtfDestinationMgr.DESTINATION_NULL, new Object[] { "RtfDestinationNull", "" } );
            }
            return instance;
        }
    }
    
    public static RtfDestination getDestination(String destination) {
        RtfDestination dest = null;
        if(destinations.containsKey(destination)) {
            dest = (RtfDestination)destinations.get(destination);
        } else {
            if(ignoreUnknownDestinations) {
                dest = (RtfDestination)destinations.get(DESTINATION_NULL);
            } else {
                dest = (RtfDestination)destinations.get(DESTINATION_DOCUMENT);
            }
        }
        dest.setParser(RtfDestinationMgr.rtfParser);
        return dest;
    }
    
    public static boolean addDestination(String destination, Object[] args) {
        if(destinations.containsKey(destination)) {
            return true;
        }
        
        String thisClass =  "com.lowagie.text.rtf.parser.destinations." + (String)args[0];

        if(thisClass.indexOf("RtfDestinationNull") >= 0) {
            destinations.put(destination, RtfDestinationNull.getInstance());
            return true;
        }
        
        Class value = null;
    
        try {
            value = Class.forName(thisClass);
        } catch (ClassNotFoundException e1) {
            
            e1.printStackTrace();
            return false;
        }
        
        RtfDestination c = null;
        
        if(destinationObjects.containsKey(value.getName())) {
            c = (RtfDestination)destinationObjects.get(value.getName());        
        } else {
            try {
                c = (RtfDestination)value.newInstance();
            } catch (InstantiationException e) {
                
                e.printStackTrace();
                return false;
            } catch (IllegalAccessException e) {
                
                e.printStackTrace();
                return false;
            }
        }
        
        c.setParser(rtfParser);
        
        if(value.isInstance(RtfDestinationInfo.class)) {
                ((RtfDestinationInfo)c).setElementName(destination);
        }
        
        if(value.isInstance(RtfDestinationStylesheetTable.class)) {
                ((RtfDestinationStylesheetTable)c).setElementName(destination);
                ((RtfDestinationStylesheetTable)c).setType((String)args[1]);
        }

        destinations.put(destination, c);
        destinationObjects.put(value.getName(), c);
        return true;
    }
    
    

    
    public static boolean addListener(String destination, RtfDestinationListener listener) {
        RtfDestination dest = getDestination(destination);
        if(dest != null) {
            return dest.addListener(listener);
        }
        return false;
    }

    
    public static boolean removeListener(String destination, RtfDestinationListener listener) {
        RtfDestination dest = getDestination(destination);
        if(dest != null) {
            return dest.removeListener(listener);
        }
        return false;
    }
}
