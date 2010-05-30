package printer.capprox;

import java.io.File;

import de.ovgu.cide.fstgen.ast.FSTNode;
import printer.ArtifactPrintVisitor;
import printer.PrintVisitorException;

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
