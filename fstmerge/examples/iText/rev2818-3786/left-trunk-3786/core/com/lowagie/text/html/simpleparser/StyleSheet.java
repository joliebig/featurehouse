

package com.lowagie.text.html.simpleparser;

import com.lowagie.text.html.Markup;
import java.util.HashMap;

public class StyleSheet {

    public HashMap classMap = new HashMap();

    public HashMap tagMap = new HashMap();

    
    public StyleSheet() {
    }

    public void applyStyle(String tag, HashMap props) {
        HashMap map = (HashMap) tagMap.get(tag.toLowerCase());
        if (map != null) {
            HashMap temp = new HashMap(map);
            temp.putAll(props);
            props.putAll(temp);
        }
        String cm = (String) props.get(Markup.HTML_ATTR_CSS_CLASS);
        if (cm == null)
            return;
        map = (HashMap) classMap.get(cm.toLowerCase());
        if (map == null)
            return;
        props.remove(Markup.HTML_ATTR_CSS_CLASS);
        HashMap temp = new HashMap(map);
        temp.putAll(props);
        props.putAll(temp);
    }

    public void loadStyle(String style, HashMap props) {
        classMap.put(style.toLowerCase(), props);
    }

    public void loadStyle(String style, String key, String value) {
        style = style.toLowerCase();
        HashMap props = (HashMap) classMap.get(style);
        if (props == null) {
            props = new HashMap();
            classMap.put(style, props);
        }
        props.put(key, value);
    }

    public void loadTagStyle(String tag, HashMap props) {
        tagMap.put(tag.toLowerCase(), props);
    }

    public void loadTagStyle(String tag, String key, String value) {
        tag = tag.toLowerCase();
        HashMap props = (HashMap) tagMap.get(tag);
        if (props == null) {
            props = new HashMap();
            tagMap.put(tag, props);
        }
        props.put(key, value);
    }

}