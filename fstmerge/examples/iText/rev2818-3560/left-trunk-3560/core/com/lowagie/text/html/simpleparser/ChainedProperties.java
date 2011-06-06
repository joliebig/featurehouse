

package com.lowagie.text.html.simpleparser;

import com.lowagie.text.ElementTags;
import java.util.ArrayList;
import java.util.HashMap;

public class ChainedProperties {

    public final static int fontSizes[] = { 8, 10, 12, 14, 18, 24, 36 };

    public ArrayList chain = new ArrayList();

    
    public ChainedProperties() {
    }

    public String getProperty(String key) {
        for (int k = chain.size() - 1; k >= 0; --k) {
            Object obj[] = (Object[]) chain.get(k);
            HashMap prop = (HashMap) obj[1];
            String ret = (String) prop.get(key);
            if (ret != null)
                return ret;
        }
        return null;
    }

    public boolean hasProperty(String key) {
        for (int k = chain.size() - 1; k >= 0; --k) {
            Object obj[] = (Object[]) chain.get(k);
            HashMap prop = (HashMap) obj[1];
            if (prop.containsKey(key))
                return true;
        }
        return false;
    }

    public void addToChain(String key, HashMap prop) {
        
        String value = (String) prop.get(ElementTags.SIZE);
        if (value != null) {
            if (value.endsWith("px")) {
                prop.put(ElementTags.SIZE, value.substring(0,
                        value.length() - 2));
            } else {
                int s = 0;
                if (value.startsWith("+") || value.startsWith("-")) {
                    String old = getProperty("basefontsize");
                    if (old == null)
                        old = "12";
                    float f = Float.parseFloat(old);
                    int c = (int) f;
                    for (int k = fontSizes.length - 1; k >= 0; --k) {
                        if (c >= fontSizes[k]) {
                            s = k;
                            break;
                        }
                    }
                    int inc = Integer.parseInt(value.startsWith("+") ? value
                            .substring(1) : value);
                    s += inc;
                } else {
                    try {
                        s = Integer.parseInt(value) - 1;
                    } catch (NumberFormatException nfe) {
                        s = 0;
                    }
                }
                if (s < 0)
                    s = 0;
                else if (s >= fontSizes.length)
                    s = fontSizes.length - 1;
                prop.put(ElementTags.SIZE, Integer.toString(fontSizes[s]));
            }
        }
        chain.add(new Object[] { key, prop });
    }

    public void removeChain(String key) {
        for (int k = chain.size() - 1; k >= 0; --k) {
            if (key.equals(((Object[]) chain.get(k))[0])) {
                chain.remove(k);
                return;
            }
        }
    }
}
