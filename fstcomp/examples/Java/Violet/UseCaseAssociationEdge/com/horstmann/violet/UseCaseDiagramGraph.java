package com.horstmann.violet;

public class UseCaseDiagramGraph
{
   static
   {
      ClassRelationshipEdge communication =
         new ClassRelationshipEdge();
      communication.setBentStyle(BentStyle.STRAIGHT);
      communication.setLineStyle(LineStyle.SOLID);
      communication.setEndArrowHead(ArrowHead.NONE);
      EDGE_PROTOTYPES[0] = communication;
   }
}





