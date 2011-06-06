package de.masters_of_disaster.ant.tasks.ar;



public interface ArConstants {
    
    int    NAMELEN = 16;

    
    int    FILEDATELEN = 12;

    
    int    UIDLEN = 6;

    
    int    GIDLEN = 6;

    
    int    MODELEN = 8;

    
    int    SIZELEN = 10;

    
    int    MAGICLEN = 2;

    
    String HEADERMAGIC = "`\n";

    
    int    HEADERLENGTH = NAMELEN + FILEDATELEN + UIDLEN + GIDLEN + MODELEN + SIZELEN + MAGICLEN;

    
    byte[] PADDING = { '\n' };

    
    byte[] ARMAGIC = { '!', '<', 'a', 'r', 'c', 'h', '>', '\n' };
}
