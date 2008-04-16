//created on: Thu Oct 13 21:36:48 CDT 2005

import java.io.FileInputStream;
import Jakarta.util.Util;
import org.jdom.*;

public class CubeInterfaceTester {


    /**
     * Test runner for verifying correct functionality of
     * CubeInterface layer added onto guidsl
     *
     * Additional usage methods CubeInterface's provides
     *
     *  Model File -> XML
     *  guidsl.Main.getModelXML()
     *
     *  XML -> Model File
     *  guidsl.CubeXMLHandler.generateModelFile(Element model)
     *
     *  XML -> Model File Content as String
     *  guidsl.CubeXMLHandler.generateModelFileContents(Element model)
     *
     *  Guidsl was designed to be used with only one model file at a time
     *  this method resets members in the production class.
     *  production.resetModel()
     *
     *
     */
    public static void main(String[] args) {

        FileInputStream  inputFile = null;
        try {
            if (args.length==0)
                inputFile = new FileInputStream( "java5.m" );
            else
                inputFile = new FileInputStream( args[0] );
        }
        catch ( Exception e ) {
            e.printStackTrace();
            System.exit(0);
        }


        String xml = getModelFileContentAsXML(inputFile);

        Document doc = XMLUtils.getDocFromString(xml);
        Element model = doc.getRootElement();

        String fileName=null;
        try {
            //String fileContent = guidsl.CubeXMLHandler.generateModelFileContent(model);
            //System.out.println(fileContent);

            fileName = CubeXMLHandler.generateModelFile(model);
        } catch (Exception e){};

        //---------------------------
        production.resetModel();

        Tool tool = new Tool( fileName );
        SATtest t = new SATtest( fileName, true, true);



        boolean satTestResult = (tool.modelDebug( t, true ));

        System.out.println("Layer passed all tests. (if no error reported till now)");

    }

    private static String getModelFileContentAsXML(FileInputStream inputFile){

        if (inputFile==null)
            return "No file given";

        try {
            Parser myParser = Parser.getInstance( inputFile );
            Model    inputRoot = ( Model ) myParser.parseAll() ;
            Main.process( inputRoot );
            return Main.getModelXML();
        }
        catch( SemanticException e ) {
             int errorCnt = Util.errorCount();
             System.err.println( Util.errorCount() + " error(s) found");
             System.err.println( "Processing terminated" );
             System.out.println( Util.errorCount() + " error(s) found. Processing terminated");
        }
        catch ( Exception e ) {
            e.printStackTrace();
        }
        return "";
    }
}
