package com.horstmann.violet;

public class UseCaseDiagramGraph
{
   static
   {
      ClassRelationshipEdge generalization =
         new ClassRelationshipEdge();
      generalization.setBentStyle(BentStyle.STRAIGHT);
      generalization.setLineStyle(LineStyle.SOLID);
      generalization.setEndArrowHead(ArrowHead.TRIANGLE);
      EDGE_PROTOTYPES[3] = generalization;
   }
}





