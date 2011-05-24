package com.horstmann.violet.framework;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.print.Book;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import java.util.ResourceBundle;

import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.print.attribute.PrintRequestAttributeSet;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
   
   /**
    * The component for displaying the print preview.
    */
   class PrintPreviewCanvas extends JComponent
   {
   		private PrintDialog pd;
   		PrintPreviewCanvas(PrintDialog pd_) {
   			pd = pd_;
   		}
   
      public Dimension getPreferredSize()
      {
         return new Dimension(DEFAULT_WIDTH, DEFAULT_HEIGHT);
      }

      public void paintComponent(Graphics g)
      {
         Graphics2D g2 = (Graphics2D) g;
         pd.bounds = pd.graph.getBounds(g2);

         double xoff; // x offset of page start in window
         double yoff; // y offset of page start in window
         double scalePagesToCanvas; // scale factor to fit pages in canvas

         double px = pd.pageFormat.getImageableWidth();
         double py = pd.pageFormat.getImageableHeight();

         int cols = pd.getCols();
         int rows = pd.getRows();

         double dx = px * pd.getCols();
         double dy = py * pd.getRows();

         double sx = pd.getWidth() - 1;
         double sy = pd.getHeight() - 1;
         if (dx / dy < sx / sy) // center horizontally
         {
            scalePagesToCanvas = sy / dy;
            xoff = 0.5 * (sx - scalePagesToCanvas * dx);
            yoff = 0;
         }
         else
         // center vertically
         {
            scalePagesToCanvas = sx / dx;
            xoff = 0;
            yoff = 0.5 * (sy - scalePagesToCanvas * dy);
         }
         g2.translate((float) xoff, (float) yoff);
         g2.scale((float) scalePagesToCanvas, (float) scalePagesToCanvas);
         // draw page backgrounds
         Rectangle2D pages = new Rectangle2D.Double(0, 0, px * cols, py * rows);
         g2.setPaint(Color.WHITE);
         g2.fill(pages);
         g2.setPaint(Color.BLACK);

         AffineTransform oldTransform = g2.getTransform();

         g2.scale((float) pd.scaleGraph, (float) pd.scaleGraph);
         g2.translate((float) -pd.bounds.getX(), (float) -pd.bounds.getY());
         pd.graph.draw(g2, null);

         g2.setTransform(oldTransform);
         // draw page outlines (ignoring margins)
         g2.setPaint(getBackground());
         for (int i = 0; i < cols; i++)
            for (int j = 0; j < rows; j++)
            {
               Rectangle2D page = new Rectangle2D.Double(i * px, j * py, px, py);
               g2.draw(page);
            }
      }
      private static final int DEFAULT_WIDTH = 450;
      private static final int DEFAULT_HEIGHT = 300;
   }