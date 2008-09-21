module Data.Graph.Inductive.Example
       (e3')
       where
{  
	import Data.Graph.Inductive.Monad;
  	import Data.Graph.Inductive.Monad.IOArray;

  e3'
    = mkGraphM (genUNodes 2) [(1, 2, "a"), (1, 2, "b"), (1, 2, "a")]
}
