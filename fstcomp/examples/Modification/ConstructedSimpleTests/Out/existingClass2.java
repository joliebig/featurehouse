
package Graph; import java.util.List; 
 
class  existingClass2 {
	 private String existingField;

	public void newMethod__wrappee__ClassicFeature(){System.out.println("bim");}

	public void newMethod()	{newMethod__wrappee__ClassicFeature();System.out.println("bäm");}

	 void existingMethod__wrappee__ClassicFeature() {	System.out.print("classic feature"); }

	 void existingMethod() {	existingMethod__wrappee__ClassicFeature();	System.out.println("print method expansion from AuxClass.java"); }


}
