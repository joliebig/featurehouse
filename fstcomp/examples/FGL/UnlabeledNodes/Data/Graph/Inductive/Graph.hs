module Data.Graph.Inductive.Graph
       (UNode, UContext, UDecomp, UPath)
       where
{ import Data.List (sortBy);
   
  type UNode = LNode ();
   
  type UPath = [UNode];
   
  type UContext = ([Node], Node, [Node]);
   
  type UDecomp g = (Maybe UContext, g)
}
