package com.lowagie.text.rtf.graphic;

import java.awt.Color;
import java.awt.Point;
import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.DocumentException;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Image;
import com.lowagie.text.rtf.RtfAddableElement;


public class RtfShapeProperty extends RtfAddableElement {
    
    public static final String PROPERTY_IMAGE = "pib";
    
    public static final String PROPERTY_VERTICIES = "pVerticies";
    
    public static final String PROPERTY_GEO_TOP = "geoTop";
    
    public static final String PROPERTY_GEO_LEFT = "geoLeft";
    
    public static final String PROPERTY_GEO_RIGHT = "geoRight";
    
    public static final String PROPERTY_GEO_BOTTOM = "geoBottom";
    
    public static final String PROPERTY_LAYOUT_IN_CELL = "fLayoutInCell";
    
    public static final String PROPERTY_FLIP_V = "fFlipV";
    
    public static final String PROPERTY_FLIP_H = "fFlipH";
    
    public static final String PROPERTY_FILL_COLOR = "fillColor";
    
    public static final String PROPERTY_LINE_COLOR = "lineColor";
    
    public static final String PROPERTY_ADJUST_VALUE = "adjustValue";

    
    private static final int PROPERTY_TYPE_LONG = 1;
    
    private static final int PROPERTY_TYPE_BOOLEAN = 2;
    
    private static final int PROPERTY_TYPE_DOUBLE = 3;
    
    private static final int PROPERTY_TYPE_COLOR = 4;
    
    private static final int PROPERTY_TYPE_ARRAY = 5;
    
    private static final int PROPERTY_TYPE_IMAGE = 6;
    
    
    private int type = 0;
    
    private String name = "";
    
    private Object value = null;
    
    
    private RtfShapeProperty(String name, Object value) {
        this.name = name;
        this.value = value;
    }
    
    
    public RtfShapeProperty(String name, long value) {
        this(name, new Long(value));
        this.type = PROPERTY_TYPE_LONG;
    }
    
    
    public RtfShapeProperty(String name, double value) {
        this(name, new Double(value));
        this.type = PROPERTY_TYPE_DOUBLE;
    }
    
    
    public RtfShapeProperty(String name, boolean value) {
        this(name, Boolean.valueOf(value));
        this.type = PROPERTY_TYPE_BOOLEAN;
    }
    
    
    public RtfShapeProperty(String name, Color value) {
        this(name, (Object) value);
        this.type = PROPERTY_TYPE_COLOR;
    }
    
    
    public RtfShapeProperty(String name, int[] value) {
        this(name, (Object) value);
        this.type = PROPERTY_TYPE_ARRAY;
    }
    
    
    public RtfShapeProperty(String name, Point[] value) {
        this(name, (Object) value);
        this.type = PROPERTY_TYPE_ARRAY;
    }
    
    
    public RtfShapeProperty(String name, Image value) {
        this.name = name;
        this.value = value;
        this.type = PROPERTY_TYPE_IMAGE;
    }

    
    public String getName() {
        return this.name;
    }
    
    
    public void writeContent(final OutputStream result) throws IOException
    {        
        result.write(OPEN_GROUP);
        result.write("\\sp".getBytes());
        result.write(OPEN_GROUP);
        result.write("\\sn".getBytes());
        result.write(DELIMITER);
        result.write(this.name.getBytes());
        result.write(CLOSE_GROUP);
        result.write(OPEN_GROUP);
        result.write("\\sv".getBytes());
        result.write(DELIMITER);
        switch(this.type) {
        case PROPERTY_TYPE_LONG: 
        case PROPERTY_TYPE_DOUBLE:
            result.write(this.value.toString().getBytes());
            break;
        case PROPERTY_TYPE_BOOLEAN:
            if(((Boolean) this.value).booleanValue()) {
                result.write("1".getBytes());
            } else {
                result.write("0".getBytes());
            }
            break;
        case PROPERTY_TYPE_COLOR:
            Color color = (Color) this.value;
            result.write(intToByteArray(color.getRed() | (color.getGreen() << 8) | (color.getBlue() << 16)));
            break;
        case PROPERTY_TYPE_ARRAY:
            if(this.value instanceof int[]) {
                int[] values = (int[]) this.value;
                result.write("4;".getBytes());
                result.write(intToByteArray(values.length));
                result.write(COMMA_DELIMITER);
                for(int i = 0; i < values.length; i++) {
                    result.write(intToByteArray(values[i]));
                    if(i < values.length - 1) {
                        result.write(COMMA_DELIMITER);
                    }
                }
            } else if(this.value instanceof Point[]) {
                Point[] values = (Point[]) this.value;
                result.write("8;".getBytes());
                result.write(intToByteArray(values.length));
                result.write(COMMA_DELIMITER);
                for(int i = 0; i < values.length; i++) {
                    result.write("(".getBytes());
                    result.write(intToByteArray(values[i].x));
                    result.write(",".getBytes());
                    result.write(intToByteArray(values[i].y));
                    result.write(")".getBytes());
                    if(i < values.length - 1) {
                        result.write(COMMA_DELIMITER);
                    }
                }
            }
            break;
        case PROPERTY_TYPE_IMAGE:
            Image image = (Image)this.value;
            RtfImage img = null;
            try {
                img = new RtfImage(this.doc, image);
            }
            catch (DocumentException de) {
                throw new ExceptionConverter(de);
            }
            img.setTopLevelElement(true);
            result.write(OPEN_GROUP);
            img.writeContent(result);
            result.write(CLOSE_GROUP);
            break;
        }
        result.write(CLOSE_GROUP);
        result.write(CLOSE_GROUP);
    }
    
}
