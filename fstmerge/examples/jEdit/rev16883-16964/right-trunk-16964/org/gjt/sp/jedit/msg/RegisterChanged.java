

package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.EBComponent;
import org.gjt.sp.jedit.EBMessage;
import org.gjt.sp.jedit.Registers;


public class RegisterChanged extends EBMessage
{
	 private char registerName;

	 
	 public RegisterChanged(EBComponent source, char name)
	 {
		 super(source);
		 registerName = name;
	 }
	 
	 public char getRegisterName()
	 {
		 return registerName;
	 }
	 
	 public String getRegisterValue()
	 {
		 return Registers.getRegister(registerName).toString();
	 }
	 
	 public String paramString()
	 {
		 return "register=" + registerName + "," + super.paramString();
	 }
}
