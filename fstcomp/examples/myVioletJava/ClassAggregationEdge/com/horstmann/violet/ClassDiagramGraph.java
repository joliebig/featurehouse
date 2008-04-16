package com.horstmann.violet;

public class ClassDiagramGraph
{
   	static {
      ClassRelationshipEdge aggregation = new ClassRelationshipEdge();
      aggregation.setBentStyle(BentStyle.HVH);
      aggregation.setStartArrowHead(ArrowHead.DIAMOND);
      EDGE_PROTOTYPES[4] = aggregation;
   	}
}
