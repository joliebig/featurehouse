
package com.lowagie.tools;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;

import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;


public class BuildTutorial {

    static String root;
    static FileWriter build;
    
    
    

    

    public static void main(String[] args) {
        if (args.length == 4) {
            File srcdir = new File(args[0]);
            File destdir = new File(args[1]);
            File xsl_examples = new File(srcdir, args[2]);
            File xsl_site = new File(srcdir, args[3]);
            try {
                System.out.print("Building tutorial: ");
                root = new File(args[1], srcdir.getName()).getCanonicalPath();
                System.out.println(root);
                build = new FileWriter(new File(root, "build.xml"));
                build.write("<project name=\"tutorial\" default=\"all\" basedir=\".\">\n");
                build.write("<target name=\"all\">\n");
                action(srcdir, destdir, xsl_examples, xsl_site);
                build.write("</target>\n</project>");
                build.flush();
                build.close();
            }
            catch(IOException ioe) {
                ioe.printStackTrace();
            }
        } else {
            System.err
                    .println("Wrong number of parameters.\nUsage: BuildSite srcdr destdir xsl_examples xsl_site");
        }
    }

    
    public static void action(File source, File destination, File xsl_examples, File xsl_site) throws IOException {
        if (".svn".equals(source.getName())) return;
        System.out.print(source.getName());
        if (source.isDirectory()) {
            System.out.print(" ");
            System.out.println(source.getCanonicalPath());
            File dest = new File(destination, source.getName());
            dest.mkdir();
            File current;
            File[] xmlFiles = source.listFiles();
            if (xmlFiles != null) {
                for (int i = 0; i < xmlFiles.length; i++) {
                    current = xmlFiles[i];
                    action(current, dest, xsl_examples, xsl_site);
                }
            }
            else {
                System.out.println("... skipped");
            }
        }
        else if (source.getName().equals("index.xml")) {
            System.out.println("... transformed");
            convert(source, xsl_site, new File(destination, "index.php"));
            File buildfile = new File(destination, "build.xml");
            String path = buildfile.getCanonicalPath().substring(root.length());
            path = path.replace(File.separatorChar, '/');
            if ("/build.xml".equals(path)) return;
            convert(source, xsl_examples, buildfile);
            build.write("\t<ant antfile=\"${basedir}");
            build.write(path);
            build.write("\" target=\"install\" inheritAll=\"false\" />\n");
        }
        else {
            System.out.println("... skipped");
        }
    }
    
    
    public static void convert(File infile, File xslfile, File outfile) {
        try {
            
            TransformerFactory factory = TransformerFactory.newInstance();

            
            Templates template = factory.newTemplates(new StreamSource(
                    new FileInputStream(xslfile)));

            
            Transformer xformer = template.newTransformer();
            
            
            String branch = outfile.getParentFile().getCanonicalPath().substring(root.length());
            branch = branch.replace(File.separatorChar, '/');
            StringBuffer path = new StringBuffer();
            for (int i = 0; i < branch.length(); i++) {
                if (branch.charAt(i) == '/') path.append("/..");
            }
            
            xformer.setParameter("branch", branch);
            xformer.setParameter("root", path.toString());

            
            Source source = new StreamSource(new FileInputStream(infile));
            Result result = new StreamResult(new FileOutputStream(outfile));

            
            
            xformer.transform(source, result);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

