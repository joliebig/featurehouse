

package com.lowagie.text.html.simpleparser;

import java.util.HashMap;

import com.lowagie.text.DocListener;
import com.lowagie.text.Image;


public interface Img {
    boolean process(Image img, HashMap h, ChainedProperties cprops, DocListener doc);
}
