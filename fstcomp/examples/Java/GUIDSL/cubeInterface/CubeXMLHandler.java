//created on: Thu Oct 13 19:35:26 CDT 2005
import org.jdom.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Iterator;
import java.util.Vector;
import java.io.File;
import java.io.IOException;
import java.io.FileWriter;

/** This is the class responsible for taking a JDOM tree's root Element (representing
    the Model node) and convert it back to plain text model in a format that GUI dsl
    can understand
*/
public class CubeXMLHandler{

    public static String generateModelFileContent(Element model){
        StringBuffer out=new StringBuffer();
        out.append(generateGrammar(model.getChild("grammar")));

        String constraints = generateConstraints(model.getChild("constraints"));
        if (constraints.trim().length()>0)
            out.append("\n\n%%\n"+constraints);

        return out.toString();
    }


    public static String generateModelFile(Element model) throws IOException{
        StringBuffer out=new StringBuffer();

        out.append(generateGrammar(model.getChild("grammar")));

        String constraints = generateConstraints(model.getChild("constraints"));
        if (constraints.trim().length()>0)
            out.append("\n\n%%\n"+constraints);

        try {
            File f = File.createTempFile("tempModel",".m");

            if(!f.canWrite())throw new IOException("cant write in file");

            FileWriter fw = new FileWriter(f);
            fw.write(out.toString());
            fw.close();
            return f.getAbsolutePath();
        }
        catch(IOException ioe){
            System.err.println(ioe);
            throw ioe;
        }

    }

    static String generateGrammar(Element grammar){
        StringBuffer out=new StringBuffer();

        List prods = grammar.getChildren("production");
        Iterator it = prods.iterator();

        while(it.hasNext()){
            Element production = (Element)it.next();
            out.append(generateProduction(production));
        }

        return out.toString();
    }

    static String generateProduction(Element prod){
        StringBuffer out = new StringBuffer();

        out.append(prod.getAttributeValue("name")+" : ");

        List patterns = prod.getChildren("pattern");
        Iterator it = patterns.iterator();
        int count=0;
        while(it.hasNext()){
            Element pattern = (Element)it.next();

            if(count>0)
                out.append("\n\t | ");

            out.append(generatePattern(pattern));


            ++count;
        }

        out.append(" ; \n\n");

        return out.toString();
    }


    static String generatePattern(Element pattern){
        StringBuffer out = new StringBuffer();

        // primitive or prod refs
        List primitive = pattern.getChildren();
        Iterator it = primitive.iterator();

        while(it.hasNext()){
            Element node = (Element)it.next();

            if(node.getAttributeValue("type").equals("optional") || node.getAttributeValue("type").equals("optional "))
                out.append("["+node.getValue()+"] ");
            else
                out.append(node.getValue()+" ");

            //if (node.getName().equals("primitive"))
            //  primitives.addElement(node.getTextTrim());
        }
        out.append(" :: "+pattern.getAttributeValue("name"));


        return out.toString();
    }


    static String generateConstraints(Element constraints){
        StringBuffer out=new StringBuffer();

        if(constraints==null)
            return "";

        List constrs=constraints.getChildren("constraint");
        Iterator it = constrs.iterator();

        while(it.hasNext()){
            Element child = (Element)((Element)it.next()).getChildren().get(0);


            if (child==null)
                continue;

            out.append(generateOP(child)+";\n");

            /*
            if (child.getName().equals("iff"))
                out.append(generateIFF(child)+";\n");
            else if (child.getName().equals("implies"))
                out.append(generateImplies(child)+";\n");
            */

        }

        return out.toString();
    }

    static String generateIFF(Element iff){
        StringBuffer out=new StringBuffer();

        List list = iff.getChildren();
        if (list.size()<2)return "error <iff> token must have two arguments";

        Element right = (Element)iff.getChildren().get(0);
        Element left = (Element)iff.getChildren().get(1);

        out.append(generateOP(left));
        out.append(" iff ");
        out.append(generateOP(right));
        return out.toString();
    }

    static String generateImplies(Element implies){
        StringBuffer out=new StringBuffer();

        List list = implies.getChildren();
        if (list.size()<2)return "error <implies> token must have two arguments";

        Element right = (Element)implies.getChildren().get(0);
        Element left = (Element)implies.getChildren().get(1);

        out.append(generateOP(left));
        out.append(" implies ");
        out.append(generateOP(right));
        return out.toString();
    }

    static String generateOP(Element node){
        StringBuffer out = new StringBuffer();

        if (node.getName().equals("and"))
            out.append(" "+generateAND(node)+" ");

        else if (node.getName().equals("or"))
            out.append(" "+generateOR(node)+" ");

        else if (node.getName().equals("not"))
            out.append(" "+generateNOT(node)+" ");

        else if (node.getName().equals("term"))
            out.append(" "+node.getText()+" ");

        else if (node.getName().equals("onlyone"))
            out.append(" "+generateOnlyone(node)+" ");
        else if (node.getName().equals("choose1"))
            out.append(" "+generateChoose1(node)+" ");

        else if (node.getName().equals("iff"))
             out.append("("+generateIFF(node)+")");
        else if (node.getName().equals("implies"))
            out.append("("+generateImplies(node)+")");

        return out.toString();
    }


    static String generateChoose1(Element node){
        StringBuffer out = new StringBuffer();

        List list = node.getChildren();
        if (list.size()!=2)return "error <choose1> token must have two arguments";

        if (node.getName().equals("term"))
            return node.getText();

        Element left = (Element)node.getChildren().get(0);
        Element right = (Element)node.getChildren().get(1);

        String temp = generateSubOnlyone(left)+","+generateSubOnlyone(right);
        out.append("(choose1("+temp+"))");


        return out.toString();
    }

    //When parsed, guidsl breaks choose1(..) into number of onlyone tokens.
    //so here I generate a choose1(..) predicate
    static String generateOnlyone(Element node){
        StringBuffer out = new StringBuffer();

        List list = node.getChildren();
        if (list.size()!=2)return "error <onlyone> token must have two arguments";

        if (node.getName().equals("term"))
            return node.getText();

        Element left = (Element)node.getChildren().get(0);
        Element right = (Element)node.getChildren().get(1);

        String temp=null;
        if(left!=null && right!=null)
          temp = generateSubOnlyone(left)+","+generateSubOnlyone(right);
        else if (right==null)
          temp = generateSubOnlyone(left);
        else {
          return "error <onlyone> has right side null";
        }

        out.append("(choose1("+temp+"))");


        return out.toString();
    }

    static String generateSubOnlyone(Element node){
        StringBuffer out = new StringBuffer();

        if (node.getName().equals("term"))
            return node.getText();

        Element left = (Element)node.getChildren().get(0);
        Element right = (Element)node.getChildren().get(1);

        if(left!=null && right!=null)
            out.append(generateSubOnlyone(left)+","+generateSubOnlyone(right));
        else if(right==null)
            out.append(generateSubOnlyone(left));
        else
            return "error <onlyone> has right side null";


        return out.toString();
    }

    static String generateAND(Element and){
        StringBuffer out = new StringBuffer();

        List list = and.getChildren();
        //if (list.size()!=2)return "error <and> token must have two arguments";
        if (list.size()<1)return "error <and> token must have atleast one argument";

        out.append("(");
        for (int i=0;i<list.size();i++){
            Element elm = (Element)list.get(i);
            if (i>0)
                out.append(" and ");
            out.append(generateOP(elm));
        }
        out.append(")");

        //Element left = (Element)and.getChildren().get(0);
        //Element right = (Element)and.getChildren().get(1);

        //out.append("("+generateOP(left));
        //out.append(" and ");
        //out.append(generateOP(right)+")");

        return out.toString();
    }

    static String generateOR(Element or){
        StringBuffer out = new StringBuffer();

        List list = or.getChildren();
        //if (list.size()!=2)return "error <and> token must have two arguments";
        if (list.size()<1)return "error <or> token must have atleast one argument";

        out.append("(");
        for (int i=0;i<list.size();i++){
            Element elm = (Element)list.get(i);
            if (i>0)
                out.append(" or ");
            out.append(generateOP(elm));
        }
        out.append(")");
        return out.toString();
    }

    static String generateNOT(Element not){
        StringBuffer out = new StringBuffer();

        List list = not.getChildren();
        if (list.size()!=1)return "error <not> token must have one arguments";

        Element left = (Element)not.getChildren().get(0);

        out.append("not"+generateOP(left));

        return out.toString();
    }

}