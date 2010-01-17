package net.sf.jabref;

import java.util.*;


public final class EntryEditorTabList {

    private List list = null;
    private List names = null;

    public EntryEditorTabList() {
        init();
    }

    private void init() {
        list = new ArrayList();
        names = new ArrayList();
        int i = 0;
        String name;
        String[] fields;
        if (Globals.prefs.hasKey(Globals.prefs.CUSTOM_TAB_NAME + 0)) {
            
            while (Globals.prefs.hasKey(Globals.prefs.CUSTOM_TAB_NAME + i)) {
                name = Globals.prefs.get(Globals.prefs.CUSTOM_TAB_NAME + i);
                fields = Globals.prefs.get(Globals.prefs.CUSTOM_TAB_FIELDS + i).split(";");
                List entry = Arrays.asList(fields);
                names.add(name);
                list.add(entry);
                i++;
            }
        } else {
            
            while (Globals.prefs.get(Globals.prefs.CUSTOM_TAB_NAME + "_def"+i) != null) {
                name = Globals.prefs.get(Globals.prefs.CUSTOM_TAB_NAME + "_def" + i);
                fields = Globals.prefs.get(Globals.prefs.CUSTOM_TAB_FIELDS + "_def" + i).split(";");
                List entry = Arrays.asList(fields);
                names.add(name);
                list.add(entry);
                i++;
            }
        }
    }

    public int getTabCount() {
        return list.size();
    }

    public String getTabName(int tab) {
        return (String) names.get(tab);
    }

    public List getTabFields(int tab) {
        return (List) list.get(tab);
    }
}
