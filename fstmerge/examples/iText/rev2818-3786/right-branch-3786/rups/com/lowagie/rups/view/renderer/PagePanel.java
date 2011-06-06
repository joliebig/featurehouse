

package com.lowagie.rups.view.renderer;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Rectangle2D;
import java.awt.image.ImageObserver;

import javax.swing.JPanel;

import com.sun.pdfview.PDFPage;


public class PagePanel extends JPanel
    implements ImageObserver, MouseListener, MouseMotionListener {

    
    private static final long serialVersionUID = -130815955294007634L;

    
    protected PDFPage currentPage;
    
    
    protected Image currentImage;
    
    
    protected int offx;

    
    protected int offy;

    
    Dimension prevSize;
    
    
    Rectangle2D clip;
   
    
    Rectangle2D prevClip;
    
    
    protected Rectangle zoomRect;
    
    
    AffineTransform deviceToPageSpaceTransformation;
    
    
    public PagePanel() {
        super();
        setFocusable(true);
        addMouseListener(this);
        addMouseMotionListener(this);
    }

    
    public int getCurrentPageNumber() {
        if (currentPage == null) return 0;
        return currentPage.getPageNumber();
    }
    
    
    public void paint(Graphics g) {
        Dimension sz= getSize();
        g.setColor(Color.DARK_GRAY);
        g.fillRect(0, 0, getWidth(), getHeight());
        if (currentImage == null) {
            
            g.setColor(Color.black);
            g.drawString("No page selected", getWidth()/2-30, getHeight()/2);
        } else {
            
            int imwid= currentImage.getWidth(null);
            int imhgt= currentImage.getHeight(null);

            
            offx = (sz.width-imwid)/2;
            offy = (sz.height-imhgt)/2;

            if ((imwid == sz.width && imhgt <= sz.height) ||
                    (imhgt == sz.height && imwid <= sz.width)) {
                g.drawImage(currentImage, offx, offy, this);
            }
            else {
                if (currentPage!=null) {
                    showPage(currentPage);
                }
            }
        }
        
        if (zoomRect!=null) {
            g.setColor(Color.red);
            g.drawRect(zoomRect.x, zoomRect.y,
               zoomRect.width, zoomRect.height);
        }
    }

    
    public synchronized void showPage(PDFPage page) {
        
        if (currentPage != null && prevSize != null) {
            currentPage.stop(prevSize.width, prevSize.height,  prevClip);
        }

        
        currentPage= page;
        
        if (page==null) {
            
            currentImage= null;
            clip= null;
            deviceToPageSpaceTransformation = null;
            repaint();
        } else {
            Dimension sz= getSize();
            if (sz.width + sz.height == 0) {
                
                return;
            }
            
            
            Rectangle2D useClip = clip;
            if (clip != null && deviceToPageSpaceTransformation != null) {
                useClip = deviceToPageSpaceTransformation.createTransformedShape(clip).getBounds2D();
            }
                               
            Dimension pageSize = page.getUnstretchedSize(sz.width, sz.height, useClip);

            
            currentImage= page.getImage(pageSize.width, pageSize.height, 
                                        useClip, this);
            
            
            deviceToPageSpaceTransformation = page.getInitialTransform(pageSize.width, 
                                                    pageSize.height,
                                                    useClip);
            try {
                deviceToPageSpaceTransformation = deviceToPageSpaceTransformation.createInverse();
            } catch (NoninvertibleTransformException nte) {
                System.out.println("Error inverting page transform!");
                nte.printStackTrace();
            }
            
            prevClip = useClip;
            prevSize = pageSize;
            
            repaint();
        }
    }

    
    public boolean imageUpdate(Image img, int infoflags, int x, int y,
                   int width, int height) {
        if ((infoflags & (SOMEBITS|ALLBITS))!=0) {
            repaint(x + offx, y + offy, width, height);
        }
        return true;
    }

    public void mouseClicked(MouseEvent arg0) {
        
        
    }

    public void mouseEntered(MouseEvent arg0) {
        
        
    }

    public void mouseExited(MouseEvent arg0) {
        
        
    }

    public void mousePressed(MouseEvent arg0) {
        
        
    }

    public void mouseReleased(MouseEvent arg0) {
        
        
    }

    public void mouseDragged(MouseEvent arg0) {
        
        
    }

    public void mouseMoved(MouseEvent arg0) {
        
        
    }
}
