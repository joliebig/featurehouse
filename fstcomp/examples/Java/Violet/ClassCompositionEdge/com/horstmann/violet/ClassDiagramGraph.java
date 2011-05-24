package com.horstmann.violet;

public class ClassDiagramGraph
{
   	static {
      ClassRelationshipEdge composition = new ClassRelationshipEdge();
      composition.setBentStyle(BentStyle.HVH);
      composition.setStartArrowHead(ArrowHead.BLACK_DIAMOND);
      EDGE_PROTOTYPES[5] = composition;
   	}
}
