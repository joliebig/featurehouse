//created on: Thu Nov 03 08:58:01 CST 2005

class SATSolver{

  String decode(boolean ret) {

       StringBuffer buf = new StringBuffer();

       if (isSat) {
         int[] model = solver.model();
         //System.out.println("v "+reader.decode(model));

         //System.out.println( "Solutions");

            for (int i = 0; i<model.length; i++ ) {
               if (model[i] > 0)
                 buf.append("["+model[i]+"]"+variable.findVar( model[i] )+" ");

               //System.out.println(model[i]);
           }
        }

        return buf.toString();
    }

}