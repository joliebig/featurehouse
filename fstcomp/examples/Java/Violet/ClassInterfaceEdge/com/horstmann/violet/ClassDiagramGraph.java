package com.horstmann.violet;

public class ClassDiagramGraph
{
   	static {
      ClassRelationshipEdge interfaceInheritance = new ClassRelationshipEdge();
      interfaceInheritance.setBentStyle(BentStyle.VHV);
      interfaceInheritance.setLineStyle(LineStyle.DOTTED);
      interfaceInheritance.setEndArrowHead(ArrowHead.TRIANGLE);
      EDGE_PROTOTYPES[2] = interfaceInheritance;
   	}
}
