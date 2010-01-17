

package net.sf.jabref.groups;

import net.sf.jabref.Globals;

public class UnsupportedVersionException extends Exception {
    public UnsupportedVersionException(String groupType, int version) {
        super(Globals.lang("Unsupported version of class %0: %1", groupType, ""
                + version));
    }
}
