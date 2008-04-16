package com.horstmann.violet;

public class ClassDiagramGraph
{
   	static {
   	  ClassRelationshipEdge dependency = new ClassRelationshipEdge();
      dependency.setLineStyle(LineStyle.DOTTED);
      dependency.setEndArrowHead(ArrowHead.V);
      EDGE_PROTOTYPES[0] = dependency;
   	}
}
