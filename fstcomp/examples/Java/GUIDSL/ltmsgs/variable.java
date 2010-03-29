import javax.swing.*;

class variable {
   boolean userSet = false;         // true if set by user, false otherwise
   String explanation = "";


   String explainValue()    // returns explanation of why variable has its value
   {
        return explanation;
   }


   public void print()
   {
     original();
     System.out.print("userSet is " + userSet);
   }
}