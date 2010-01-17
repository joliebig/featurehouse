

package net.sf.jabref.groups;

import java.util.Vector;

import net.sf.jabref.BibtexDatabase;
import net.sf.jabref.Globals;
import net.sf.jabref.Util;


public class VersionHandling {
    public static final int CURRENT_VERSION = 3;

    
    public static GroupTreeNode importFlatGroups(Vector<String> groups)
            throws IllegalArgumentException {
        GroupTreeNode root = new GroupTreeNode(new AllEntriesGroup());
        final int number = groups.size() / 3;
        String name, field, regexp;
        for (int i = 0; i < number; ++i) {
            field = (String) groups.get(3 * i + 0);
            name = (String) groups.get(3 * i + 1);
            regexp = (String) groups.get(3 * i + 2);
            root.add(new GroupTreeNode(new KeywordGroup(name, field, regexp,
                    false, true, AbstractGroup.INDEPENDENT)));
        }
        return root;
    }

    public static GroupTreeNode importGroups(Vector<String> orderedData,
            BibtexDatabase db, int version) throws Exception {
        switch (version) {
        case 0:
        case 1:
            return Version0_1.fromString(orderedData.firstElement(),
                    db, version);
        case 2:
        case 3:
            return Version2_3.fromString(orderedData, db, version);
        default:
            throw new IllegalArgumentException(Globals.lang(
                    "Failed to read groups data (unsupported version: %0)",
                    "" + version));
        }
    }

    
    private static class Version0_1 {
        
        private static GroupTreeNode fromString(String s, BibtexDatabase db,
                int version) throws Exception {
            GroupTreeNode root = null;
            GroupTreeNode newNode;
            int i;
            String g;
            while (s.length() > 0) {
                if (s.startsWith("(")) {
                    String subtree = getSubtree(s);
                    newNode = fromString(subtree, db, version);
                    
                    
                    
                    
                    i = 3 + subtree.length();
                    s = i >= s.length() ? "" : s.substring(i);
                } else {
                    i = indexOfUnquoted(s, ',');
                    g = i < 0 ? s : s.substring(0, i);
                    if (i >= 0)
                        s = s.substring(i + 1);
                    else
                        s = "";
                    newNode = new GroupTreeNode(AbstractGroup.fromString(Util
                            .unquote(g, '\\'), db, version));
                }
                if (root == null) 
                    root = newNode;
                else
                    root.add(newNode);
            }
            return root;
        }

        
        private static String getSubtree(String s) {
            int i = 1;
            int level = 1;
            while (i < s.length()) {
                switch (s.charAt(i)) {
                case '\\':
                    ++i;
                    break;
                case '(':
                    ++level;
                    break;
                case ')':
                    --level;
                    if (level == 0)
                        return s.substring(1, i);
                    break;
                }
                ++i;
            }
            return "";
        }

        
        private static int indexOfUnquoted(String s, char c) {
            int i = 0;
            while (i < s.length()) {
                if (s.charAt(i) == '\\') {
                    ++i; 
                } else {
                    if (s.charAt(i) == c)
                        return i;
                }
                ++i;
            }
            return -1;
        }
    }
    
    private static class Version2_3 {
        private static GroupTreeNode fromString(Vector<String> data, BibtexDatabase db,
                int version) throws Exception {
            GroupTreeNode cursor = null;
            GroupTreeNode root = null;
            GroupTreeNode newNode;
            AbstractGroup group;
            int spaceIndex;
            int level;
            String s;
            for (int i = 0; i < data.size(); ++i) {
                s = data.elementAt(i).toString();
                spaceIndex = s.indexOf(' ');
                if (spaceIndex <= 0)
                    throw new Exception("bad format"); 
                level = Integer.parseInt(s.substring(0, spaceIndex));
                group = AbstractGroup.fromString(s.substring(spaceIndex + 1),
                        db, version);
                newNode = new GroupTreeNode(group);
                if (cursor == null) {
                    
                    cursor = newNode;
                    root = cursor;
                } else {
                    
                    while (level <= cursor.getLevel())
                        cursor = (GroupTreeNode) cursor.getParent();
                    cursor.add(newNode);
                    cursor = newNode;
                }
            }
            return root;
        }
    }
}
