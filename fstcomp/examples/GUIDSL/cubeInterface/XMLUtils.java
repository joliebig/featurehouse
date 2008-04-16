//created on: Thu Oct 13 19:13:23 CDT 2005

import java.io.StringWriter;
import java.io.StringReader;
import org.jdom.Document;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;
import org.jdom.input.SAXBuilder;
import org.jdom.JDOMException;
import java.io.IOException;

/**
 * @author Sahil
 * Just some XML formattign utilities
 */
public class XMLUtils {

    public static String getXMLStrFromDoc(Document doc){

         XMLOutputter prettyOutput = new XMLOutputter(Format.getPrettyFormat());

         StringWriter stw = new StringWriter();
         try {
             prettyOutput.output(doc, stw);
             stw.close();
         } catch(Exception e) {return "Exception in writing xml to string";}

         return stw.toString();
    }

    public static String formatXMLStr(String xml){

        try {
              SAXBuilder builder = new SAXBuilder();
              org.jdom.Document result = builder.build(new StringReader(xml));
              return getXMLStrFromDoc(result);
        } catch(IOException e) {
            e.printStackTrace();

        } catch(JDOMException e) {
            e.printStackTrace();
        } catch(NullPointerException e) {
            e.printStackTrace();
        }

            return null;
    }

    public static Document getDocFromString(String xml){

        try {
              SAXBuilder builder = new SAXBuilder();
              org.jdom.Document result = builder.build(new StringReader(xml));
              return (result);
        } catch(IOException e) {
            e.printStackTrace();

        } catch(JDOMException e) {
            e.printStackTrace();
        } catch(NullPointerException e) {
            e.printStackTrace();
        }

            return null;
    }
}
