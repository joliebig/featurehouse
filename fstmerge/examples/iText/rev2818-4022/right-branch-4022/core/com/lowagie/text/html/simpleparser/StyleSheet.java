

package com.lowagie.text.html.simpleparser;

import com.lowagie.text.html.Markup;
import java.util.HashMap;

public class StyleSheet {

    public HashMap<String, HashMap<String, String>> classMap = new HashMap<String, HashMap<String, String>>();

    public HashMap<String, HashMap<String, String>> tagMap = new HashMap<String, HashMap<String, String>>();

    
    public StyleSheet() {
    }

    public void applyStyle(String tag, HashMap<String, String> props) {
        HashMap<String, String> map = tagMap.get(tag.toLowerCase());
        if (map != null) {
            HashMap<String, String> temp = new HashMap<String, String>(map);
            temp.putAll(props);
            props.putAll(temp);
        }
        String cm = props.get(Markup.HTML_ATTR_CSS_CLASS);
        if (cm == null)
            return;
        map = classMap.get(cm.toLowerCase());
        if (map == null)
            return;
        props.remove(Markup.HTML_ATTR_CSS_CLASS);
        HashMap<String, String> temp = new HashMap<String, String>(map);
        temp.putAll(props);
        props.putAll(temp);
    }

    public void loadStyle(String style, HashMap<String, String> props) {
        classMap.put(style.toLowerCase(), props);
    }

    public void loadStyle(String style, String key, String value) {
        style = style.toLowerCase();
        HashMap<String, String> props = classMap.get(style);
        if (props == null) {
            props = new HashMap<String, String>();
            classMap.put(style, props);
        }
        props.put(key, value);
    }

    public void loadTagStyle(String tag, HashMap<String, String> props) {
        tagMap.put(tag.toLowerCase(), props);
    }

    public void loadTagStyle(String tag, String key, String value) {
        tag = tag.toLowerCase();
        HashMap<String, String> props = tagMap.get(tag);
        if (props == null) {
            props = new HashMap<String, String>();
            tagMap.put(tag, props);
        }
        props.put(key, value);
    }

}