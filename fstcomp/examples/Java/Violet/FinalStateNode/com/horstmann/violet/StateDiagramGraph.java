package com.horstmann.violet;

public class StateDiagramGraph
{
   static
   {
      CircularStateNode finalState = new CircularStateNode();
      finalState.setFinal(true);
      NODE_PROTOTYPES[2] = finalState;     
   }	
}





