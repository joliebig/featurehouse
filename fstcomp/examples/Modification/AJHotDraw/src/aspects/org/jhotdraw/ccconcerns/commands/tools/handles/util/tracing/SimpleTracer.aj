package org.jhotdraw.ccconcerns.util.tracing;

import org.jhotdraw.util.Command;

/**
 * Use it for an extra (weak) check of behavior preservation
 * 
 * @author marin
 */
public aspect SimpleTracer {

	pointcut commandExecution(Command acommand) : 
		this(acommand)
		&& execution(void Command+.execute());
	
//	before(Command acommand) : commandExecution(acommand) {
//		System.out.println("before execution : " + thisJoinPoint.getSignature());
//	}
//
//	after(Command acommand) : commandExecution(acommand) {
//		System.out.println("after execution : " + thisJoinPoint.getSignature());
//	}
//
//	
////	pointcut checkDamageCall() : call(void DrawingView+.checkDamage()) && (withincode(void Command+.execute()) || within(CommandContracts));
////	
////	before(DrawingView dv) : target(dv) && checkDamageCall() {
////		System.out.println(" > "  + thisJoinPoint + " in " + dv);
////	}
////
////	after(DrawingView dv) : target(dv) && checkDamageCall() {
////		System.out.println(" < " + thisJoinPoint + " in " + dv);
////	}
//
//	before(Command cmd) : call(void DrawingView+.checkDamage()) && withincode(void Command+.execute()) && this(cmd) {
//		System.out.println(" > call from Command: " + cmd + " at " + thisJoinPoint);
//	}
//
//	before(CommandContracts cmd) : call(void DrawingView+.checkDamage()) && within(CommandContracts) && this(cmd) {
//		System.out.println(" > call from CommandContracts: " + cmd + " at " + thisJoinPoint);
//	}
//	
//	
////	pointcut viewCall() : call(* Command+.view()) && (withincode(void Command+.execute()) || within(CommandContracts));
////
////	before() : viewCall() {
////		System.out.println(" - in " + thisJoinPoint.getSignature() + " : before call  Command+.view");
////	}
////	
////	after() : viewCall() {
////		System.out.println(" - in " + thisJoinPoint.getSignature() + " : after call  Command+.view");
////	}
//
//	before(Command cmd) : call(* Command+.view()) && withincode(void Command+.execute()) && this(cmd) {
//		System.out.println(" > call from Command: " + cmd + " at " + thisJoinPoint);
//	}
//
//	before(CommandContracts cmd) : call(* Command+.view()) && within(CommandContracts) && this(cmd) {
//		System.out.println(" > call from CommandContracts: " + cmd + " at " + thisJoinPoint);
//	}

}

