


package com.lowagie.text.html.simpleparser;

import com.lowagie.text.DocListener;
import com.lowagie.text.Image;
import java.util.HashMap;

public interface ImageProvider {
    Image getImage(String src, HashMap<String, String> h, ChainedProperties cprops, DocListener doc);
}
