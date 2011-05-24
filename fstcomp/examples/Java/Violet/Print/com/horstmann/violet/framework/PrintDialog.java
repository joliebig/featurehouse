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
 * This class implements a dialog for previewing and printing a graph.
 */
public class PrintDialog extends JDialog
{
   

   /**
    * Constructs a print dialog.
    * @param gr the graph to be printed
    */
   public PrintDialog(Graph gr)
   {
      this.graph = gr;
      PrinterJob job = PrinterJob.getPrinterJob();
      pageFormat = job.defaultPage();
      attributes = new HashPrintRequestAttributeSet();
      layoutUI();
      pack();
   }

   /**
    * Lays out the UI of the dialog.
    */
   public void layoutUI()
   {
      canvas = new PrintPreviewCanvas(this);
      getContentPane().add(canvas, BorderLayout.CENTER);

      JPanel buttonPanel = new JPanel();

      ResourceFactory factory = new ResourceFactory(
         ResourceBundle.getBundle("com.horstmann.violet.framework.EditorStrings"));

      JButton printButton = factory.createButton("dialog.print.print");
      buttonPanel.add(printButton);
      printButton.addActionListener(new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
               try
               {
                  setVisible(false);
                  PrinterJob job = PrinterJob.getPrinterJob();
                  job.setPageable(makeBook());
                  if (job.printDialog(attributes))
                  {
                     pageFormat = job.validatePage(pageFormat);
                     job.print(attributes);
                  }
               }
               catch (PrinterException e)
               {
                  JOptionPane.showMessageDialog(
                     PrintDialog.this, e);
               }
            }
         });


      JButton moreButton = factory.createButton("dialog.print.more");
      buttonPanel.add(moreButton);
      moreButton.addActionListener(new ActionListener()
      {
         public void actionPerformed(ActionEvent event)
         {
            scaleGraph *= Math.sqrt(2);
            canvas.repaint();
         }
      });

      JButton fewerButton = factory.createButton("dialog.print.fewer");
      buttonPanel.add(fewerButton);
      fewerButton.addActionListener(new ActionListener()
      {
         public void actionPerformed(ActionEvent event)
         {
            scaleGraph /= Math.sqrt(2);
            canvas.repaint();
         }
      });

      JButton onePageButton = factory.createButton("dialog.print.one");
      buttonPanel.add(onePageButton);
      onePageButton.addActionListener(new ActionListener()
      {
         public void actionPerformed(ActionEvent event)
         {
            while (getRows() * getCols() > 1)
               scaleGraph /= Math.sqrt(2);
            canvas.repaint();
         }
      });


      JButton pageSetupButton = factory.createButton("dialog.print.page");
      buttonPanel.add(pageSetupButton);
      pageSetupButton.addActionListener(new
         ActionListener()
         {
            public void actionPerformed(ActionEvent event)
            {
               PrinterJob job = PrinterJob.getPrinterJob();
               PageFormat newPageFormat = job.pageDialog(attributes);
               if (newPageFormat != null)
                  pageFormat = newPageFormat;
               canvas.repaint();
            }
         });

      JButton closeButton = factory.createButton("dialog.print.close");
      buttonPanel.add(closeButton);
      closeButton.addActionListener(new ActionListener()
      {
         public void actionPerformed(ActionEvent event)
         {
            setVisible(false);
         }
      });

      getContentPane().add(buttonPanel, BorderLayout.SOUTH);
   }

   /**
    * Makes a book consisting of the pages to be printed.
    * @return the book to be printed
    */
   private Book makeBook()
   {
      Book book = new Book();
      final int pageCount = getRows() * getCols();
      Printable printable = new
         Printable()
         {
            public int print(Graphics g, PageFormat pf, int page)
               throws PrinterException
            {
               Graphics2D g2 = (Graphics2D) g;
               if (page > pageCount) return Printable.NO_SUCH_PAGE;
               g2.translate(pf.getImageableX(), pf.getImageableY());
               drawPage(g2, pf, page);
               return Printable.PAGE_EXISTS;
            }

            public void drawPage(Graphics2D g2, PageFormat pf, int page)
            {
               int cols = getCols();
               int row = page / cols;
               int col = page % cols;
               double px = pageFormat.getImageableWidth();
               double py = pageFormat.getImageableHeight();
               g2.clip(new Rectangle2D.Double(0, 0, px, py));
               g2.translate(-col * px, -row * py);
               g2.scale((float) scaleGraph, (float) scaleGraph);
               g2.translate((float) -bounds.getX(), (float) -bounds.getY());
               g2.setColor(Color.BLACK);
               g2.setBackground(Color.WHITE);
               graph.draw(g2, null);
            }
         };

      book.append(printable, pageFormat, pageCount);
      return book;
   }

   /**
    * Gets the number of columns currently required for the printout
    * @return the number of columns (>= 1)
    */
   int getCols()
   {
      return (int) Math.max(1, Math.ceil(bounds.getWidth() * scaleGraph / pageFormat.getImageableWidth()));
   }

   /**
    * Gets the number of rows currently required for the printout
    * @return the number of rows (>= 1)
    */
   int getRows()
   {
      return (int) Math.max(1, Math.ceil(bounds.getHeight() * scaleGraph / pageFormat.getImageableHeight()));
   }

   PrintPreviewCanvas canvas;
   PageFormat pageFormat;
   PrintRequestAttributeSet attributes;
   Graph graph;
   Rectangle2D bounds;
   double scaleGraph = 1;
   boolean showCropMarks;
}