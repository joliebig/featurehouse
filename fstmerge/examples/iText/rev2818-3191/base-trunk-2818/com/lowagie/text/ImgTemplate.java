

package com.lowagie.text;

import java.net.URL;

import com.lowagie.text.pdf.PdfTemplate;



public class ImgTemplate extends Image {
    
    ImgTemplate(Image image) {
        super(image);
    }
    
    
    public ImgTemplate(PdfTemplate template) throws BadElementException{
        super((URL)null);
        if (template == null)
            throw new BadElementException("The template can not be null.");
        if (template.getType() == PdfTemplate.TYPE_PATTERN)
            throw new BadElementException("A pattern can not be used as a template to create an image.");
        type = IMGTEMPLATE;
        scaledHeight = template.getHeight();
        setTop(scaledHeight);
        scaledWidth = template.getWidth();
        setRight(scaledWidth);
        setTemplateData(template);
        plainWidth = getWidth();
        plainHeight = getHeight();
    }
}