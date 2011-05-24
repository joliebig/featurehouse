class production {
   public node makef() {
      node result = original();
      switch (type) {
      case production.norm: result.incompleteMessage = "Choose 1"; break;
      case production.opt:  result.incompleteMessage = "Choose 1"; break;
      case production.star: result.incompleteMessage = "Choose 0 or more"; break;
      case production.plus: result.incompleteMessage = "Choose 1 or more"; break;
      }
      result.incompleteMessage = result.incompleteMessage + 
                                " in " + name + " panel";
      return result;
   }
}


