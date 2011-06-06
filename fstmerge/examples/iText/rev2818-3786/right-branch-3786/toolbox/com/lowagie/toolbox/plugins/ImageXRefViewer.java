



package com.lowagie.toolbox.plugins;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Cursor;
import java.awt.Toolkit;
import java.io.File;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.lowagie.text.pdf.PRStream;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStream;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.toolbox.arguments.FileArgument;
import com.lowagie.toolbox.arguments.filters.PdfFilter;
import com.lowagie.toolbox.swing.EventDispatchingThread;


public class ImageXRefViewer extends AbstractTool {

    class SpinnerListener implements ChangeListener {
        private ImageXRefViewer adaptee;
        SpinnerListener(ImageXRefViewer adaptee) {
            this.adaptee = adaptee;
        }

        
        public void stateChanged(ChangeEvent e) {
            adaptee.propertyChange(e);
        }
    }


    static {
        addVersion("$Id: ImageXRefViewer.java 3307 2008-05-01 19:55:48Z xlv $");
    }

    
    int total_number_of_pictures = 0;

    
    JSpinner jSpinner = new JSpinner();

    
    JPanel image_panel = new JPanel();

    
    CardLayout layout = new CardLayout();

    
    public ImageXRefViewer() {
        arguments.add(new FileArgument(this, "srcfile",
                "The file you want to inspect", false, new PdfFilter()));
    }

    
    protected File getDestPathPDF() throws InstantiationException {
        throw new InstantiationException("There is no file to show.");
    }

    
    protected void createFrame() {
        internalFrame = new JInternalFrame("View Image XObjects", true, false,
                true);
        internalFrame.setSize(500, 300);
        internalFrame.setJMenuBar(getMenubar());
        internalFrame.getContentPane().setLayout(new BorderLayout());

        JPanel master_panel = new JPanel();
        master_panel.setLayout(new BorderLayout());
        internalFrame.getContentPane().add(master_panel,
                java.awt.BorderLayout.CENTER);

        
        image_panel.setLayout(layout);
        jSpinner.addChangeListener(new SpinnerListener(this));
        image_panel.setBorder(BorderFactory.createEtchedBorder());

        JScrollPane scrollPane = new JScrollPane();
        scrollPane.setViewportView(image_panel);
        master_panel.add(scrollPane, java.awt.BorderLayout.CENTER);

        

        JPanel spinner_panel = new JPanel();
        spinner_panel.setLayout(new BorderLayout());
        spinner_panel.add(jSpinner, java.awt.BorderLayout.CENTER);

        JLabel image_label = new JLabel();
        image_label.setHorizontalAlignment(SwingConstants.CENTER);
        image_label.setText("images");
        spinner_panel.add(image_label, java.awt.BorderLayout.NORTH);

        master_panel.add(spinner_panel, java.awt.BorderLayout.NORTH);

        System.out.println("=== Image XObject Viewer OPENED ===");
    }

    
    public void valueHasChanged(AbstractArgument arg) {
        
    }

    
    public void propertyChange(ChangeEvent evt) {
        int picture = Integer.parseInt(jSpinner.getValue().toString());
        if (picture < 0) {
            picture = 0;
            jSpinner.setValue("0");
        }
        if (picture >= total_number_of_pictures) {
            picture = total_number_of_pictures - 1;
            jSpinner.setValue(String.valueOf(picture));
        }
        layout.show(image_panel, String.valueOf(picture));
        image_panel.repaint();
    }

    
    public static void main(String[] args) {
        ImageXRefViewer tool = new ImageXRefViewer();
        if (args.length < 1) {
            System.err.println(tool.getUsage());
        }
        tool.setMainArguments(args);
        tool.execute();
    }

    
    public void execute() {
        total_number_of_pictures = 0;
        try {
            if (getValue("srcfile") == null)
                throw new InstantiationException(
                        "You need to choose a sourcefile");
            EventDispatchingThread task = new EventDispatchingThread() {
                public Object construct() {
                    try {
                        PdfReader reader = new PdfReader(
                                ((File) getValue("srcfile")).getAbsolutePath());
                        for (int i = 0; i < reader.getXrefSize(); i++) {
                            PdfObject pdfobj = reader.getPdfObject(i);
                            if (pdfobj != null) {
                                if (pdfobj.isStream()) {
                                    PdfStream pdfdict = (PdfStream) pdfobj;
                                    PdfObject pdfsubtype = pdfdict
                                            .get(PdfName.SUBTYPE);
                                    if (pdfsubtype == null) {
                                        continue;
                                    }
                                    if (!pdfsubtype.toString().equals(
                                            PdfName.IMAGE.toString())) {
                                        continue;
                                    }
                                    System.out.println("total_number_of_pictures: "
                                            + total_number_of_pictures);
                                    System.out.println("height:"
                                            + pdfdict.get(PdfName.HEIGHT));
                                    System.out.println("width:"
                                            + pdfdict.get(PdfName.WIDTH));
                                    System.out.println("bitspercomponent:"
                                            + pdfdict.get(PdfName.BITSPERCOMPONENT));
                                    byte[] barr = PdfReader
                                            .getStreamBytesRaw((PRStream) pdfdict);
                                    java.awt.Image im = Toolkit
                                            .getDefaultToolkit().createImage(barr);
                                    javax.swing.ImageIcon ii = new javax.swing.ImageIcon(im);

                                    JLabel label = new JLabel();
                                    label.setIcon(ii);
                                    image_panel.add(label, String.valueOf(total_number_of_pictures++));
                                }
                            }
                        }
                    } catch (InstantiationException ex) {
                    } catch (IOException ex) {
                    }
                    internalFrame.setCursor(Cursor.getDefaultCursor());
                    return null;
                }
            };
            internalFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            task.start();
        } catch (Exception e) {
            JOptionPane.showMessageDialog(internalFrame, e.getMessage(), e
                    .getClass().getName(), JOptionPane.ERROR_MESSAGE);
            System.err.println(e.getMessage());
        }
    }
}
