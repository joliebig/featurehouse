package com.lowagie.text.rtf.graphic;

import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;

import com.lowagie.text.rtf.RtfAddableElement;


public class RtfShape extends RtfAddableElement {
    
    public static final int SHAPE_PICTURE_FRAME = 75;
    
    public static final int SHAPE_FREEFORM = 0;
    
    public static final int SHAPE_RECTANGLE = 1;
    
    public static final int SHAPE_ROUND_RECTANGLE = 2;
    
    public static final int SHAPE_ELLIPSE = 3;
    
    public static final int SHAPE_DIAMOND = 4;
    
    public static final int SHAPE_TRIANGLE_ISOSCELES = 5;
    
    public static final int SHAPE_TRIANGLE_RIGHT = 6;
    
    public static final int SHAPE_PARALLELOGRAM = 7;
    
    public static final int SHAPE_TRAPEZOID = 8;
    
    public static final int SHAPE_HEXAGON = 9;
    
    public static final int SHAPE_OCTAGON = 10;
    
    public static final int SHAPE_STAR = 12;
    
    public static final int SHAPE_ARROW = 13;
    
    public static final int SHAPE_ARROR_THICK = 14;
    
    public static final int SHAPE_HOME_PLATE = 15;
    
    public static final int SHAPE_CUBE = 16;
    
    public static final int SHAPE_BALLOON = 17;
    
    public static final int SHAPE_SEAL = 18;
    
    public static final int SHAPE_ARC = 19;
    
    public static final int SHAPE_LINE = 20;
    
    public static final int SHAPE_CAN = 22;
    
    public static final int SHAPE_DONUT = 23;
    
    
    public static final int SHAPE_WRAP_NONE = 0;
    
    public static final int SHAPE_WRAP_TOP_BOTTOM = 1;
    
    public static final int SHAPE_WRAP_BOTH = 2;
    
    public static final int SHAPE_WRAP_LEFT = 3;
    
    public static final int SHAPE_WRAP_RIGHT = 4;
    
    public static final int SHAPE_WRAP_LARGEST = 5;
    
    public static final int SHAPE_WRAP_TIGHT_BOTH = 6;
    
    public static final int SHAPE_WRAP_TIGHT_LEFT = 7;
    
    public static final int SHAPE_WRAP_TIGHT_RIGHT = 8;
    
    public static final int SHAPE_WRAP_TIGHT_LARGEST = 9;
    
    public static final int SHAPE_WRAP_THROUGH = 10;
    
    
    private int shapeNr = 0;
    
    private int type = 0;
    
    private RtfShapePosition position = null;
    
    private HashMap<String, RtfShapeProperty> properties = null;
    
    private int wrapping = SHAPE_WRAP_NONE;
    
    private String shapeText = "";
    
    
    public RtfShape(int type, RtfShapePosition position) {
        this.type = type;
        this.position = position;
        this.properties = new HashMap<String, RtfShapeProperty>();
    }

    
    public void setProperty(RtfShapeProperty property) {
        this.properties.put(property.getName(), property);
    }
    
    
    public void setShapeText(String shapeText) {
        this.shapeText = shapeText;
    }

    
    public void setWrapping(int wrapping) {
        this.wrapping = wrapping;
    }

        
    public void writeContent(final OutputStream result) throws IOException
    {
        this.shapeNr = this.doc.getRandomInt();
        
        this.properties.put("ShapeType", new RtfShapeProperty("ShapeType", this.type));
        if(this.position.isShapeBelowText()) {
            this.properties.put("fBehindDocument", new RtfShapeProperty("fBehindDocument", true));
        }
        if(this.inTable) {
            this.properties.put("fLayoutInCell", new RtfShapeProperty("fLayoutInCell", true));
        }
        if(this.properties.containsKey("posh")) {
            this.position.setIgnoreXRelative(true);
        }
        if(this.properties.containsKey("posv")) {
            this.position.setIgnoreYRelative(true);
        }
        
        result.write(OPEN_GROUP);
        result.write("\\shp".getBytes());
        result.write("\\shplid".getBytes());
        result.write(intToByteArray(this.shapeNr));
        this.position.writeContent(result);
        switch(this.wrapping) {
        case SHAPE_WRAP_NONE:
            result.write("\\shpwr3".getBytes());
            break;
        case SHAPE_WRAP_TOP_BOTTOM:
            result.write("\\shpwr1".getBytes());
            break;
        case SHAPE_WRAP_BOTH:
            result.write("\\shpwr2".getBytes());
            result.write("\\shpwrk0".getBytes());
            break;
        case SHAPE_WRAP_LEFT:
            result.write("\\shpwr2".getBytes());
            result.write("\\shpwrk1".getBytes());
            break;
        case SHAPE_WRAP_RIGHT:
            result.write("\\shpwr2".getBytes());
            result.write("\\shpwrk2".getBytes());
            break;
        case SHAPE_WRAP_LARGEST:
            result.write("\\shpwr2".getBytes());
            result.write("\\shpwrk3".getBytes());
            break;
        case SHAPE_WRAP_TIGHT_BOTH:
            result.write("\\shpwr4".getBytes());
            result.write("\\shpwrk0".getBytes());
            break;
        case SHAPE_WRAP_TIGHT_LEFT:
            result.write("\\shpwr4".getBytes());
            result.write("\\shpwrk1".getBytes());
            break;
        case SHAPE_WRAP_TIGHT_RIGHT:
            result.write("\\shpwr4".getBytes());
            result.write("\\shpwrk2".getBytes());
            break;
        case SHAPE_WRAP_TIGHT_LARGEST:
            result.write("\\shpwr4".getBytes());
            result.write("\\shpwrk3".getBytes());
            break;
        case SHAPE_WRAP_THROUGH:
            result.write("\\shpwr5".getBytes());
            break;
        default:
            result.write("\\shpwr3".getBytes());
        }
        if(this.inHeader) {
            result.write("\\shpfhdr1".getBytes());
        } 
        if(this.doc.getDocumentSettings().isOutputDebugLineBreaks()) {
            result.write('\n');
        }
        result.write(OPEN_GROUP);
        result.write("\\*\\shpinst".getBytes());
        for(RtfShapeProperty rsp: this.properties.values()) {
            rsp.writeContent(result);
        }
        if(!this.shapeText.equals("")) {
            result.write(OPEN_GROUP);
            result.write("\\shptxt".getBytes());
            result.write(DELIMITER);
            result.write(this.shapeText.getBytes());
            result.write(CLOSE_GROUP);
        }
        result.write(CLOSE_GROUP);
        if(this.doc.getDocumentSettings().isOutputDebugLineBreaks()) {
            result.write('\n');
        }
        result.write(CLOSE_GROUP);
        
    }        
    
}
