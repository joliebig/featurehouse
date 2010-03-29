//created on: Thu Oct 13 21:11:06 CDT 2005

class production{

    /* Rests production's member variables so that different model inputs can be handled
       by guidls at runtime
    */
    public static void resetModel(){
         if (production.Ptable != null)
            production.Ptable.clear();
        if(production.FPtable != null)
            production.FPtable.clear();

        production.counter = 0;
        production.current=null;
    }
}