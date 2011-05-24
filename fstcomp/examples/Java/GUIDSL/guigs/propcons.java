class propcons {
   public void common( gObj n ) {
      original(n);

      // supply default display string, if an override isn't present
      if (n.var.disp.equals(""))
         n.var.disp=n.var.name;
   }
}
