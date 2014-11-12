package printer.capprox;

import java.io.File;

import printer.ArtifactPrintVisitor;
import printer.PrintVisitorException;
import de.ovgu.cide.fstgen.ast.FSTNode;

public class CApproxHeaderPrintVisitor extends ArtifactPrintVisitor {
    private CApproxPrintVisitor cApproxPrintVisitor = new CApproxPrintVisitor();
    public CApproxHeaderPrintVisitor(){
	super ("H-File");
    }

    @Override
    public void processNode(FSTNode node, File folderPath)
	    throws PrintVisitorException {
	cApproxPrintVisitor.processNode(node, folderPath);
	
    }

}
