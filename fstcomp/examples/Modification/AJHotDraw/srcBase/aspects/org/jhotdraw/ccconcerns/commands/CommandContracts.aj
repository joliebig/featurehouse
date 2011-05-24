package org.jhotdraw.ccconcerns.commands;

import org.jhotdraw.framework.JHotDrawRuntimeException;
import org.jhotdraw.standard.AbstractCommand;

/**
 * This aspect groups several crosscutting policies implemented by the 
 * Command elements:
 * - ConsistentBehavior (CB) - PreExecutionCheck-ViewNotNull:  
 * 		The execute() methods in all the concrete, non-anonymous, 
 * 		non-abstract command classes that extend AbstractCommand, 
 * 		will first check the reference to the active view for non-null
 * 		values, before executing thier core logic.
 *   (Note 1: This is a CE instance, but for the sake of simplicity, we discuss it as CB)
 *   (Note 2: Originally this concern was implemented as a call to the 
 * 		super's functionality - super.execute() - that checked the reference.)
 * 
 * - ConsistentBehavior (CB) - PostExecutionNotification-UpdateToCmdChanges: 
 * 		The execute() methods in all the concrete, non-anonymous, 
 * 		non-abstract command classes that extend AbstractCommand, 
 * 		will notify the drawing view at the completion of their execution.
 * 
 * 
 * @author Marius M.
 *   
 */
public aspect CommandContracts {

	//Check the view's reference before command execution
	
    /**
	 * The poincut that captures the execution of all
	 * execute() methods in the non-anonymous subclasses of
	 * AbstractCommand, and AbstractCommand itself.
	 * NOTE: There is an AspectJ poincut definition limitation
	 * that does not allow to consistently exclude the anonymous
	 * classes form the Command hierarchy specification of the 
	 * pointcut. For this reason, naming convention and code info
	 * (such as observing the classes enclosing the anonymous commands) 
	 * can be used to define the pointcut.
 	 * NOTE2: Actually there is a way, using reflection :P - Gijs
     */
    pointcut commandExecuteCheckView(AbstractCommand acommand) :
		execution(void AbstractCommand.execute())
		&& this(acommand)
		&& !if(acommand.getClass().isAnonymousClass());
//    	// exclude the anonymous commands - no clean way to do it, so go for the enclosing types
//    	&& !within(*..DrawApplication.*)
//    	&& !within(*..CTXWindowMenu.*)
//    	&& !within(*..WindowMenu.*)
//    	&& !within(*..JavaDrawApp.*);
	
	/**
	 * The code checking the reference is crosscutting - 
	 * it was moved from AbstractCommand.execute() to
	 * this advice. The advice implements the pre-condition
	 * check in the Command elements. 
	 * (Note: Limitation - calls to super's method are not possible.)
	 */
	before(AbstractCommand acommand) : commandExecuteCheckView(acommand) {

//		System.out.println("execution : "  
//				+ thisJoinPointStaticPart.getSignature());

		if (acommand.view() == null) {
			throw new JHotDrawRuntimeException("execute should NOT be getting called when view() == null");
		};

	}

	//Notify the view of changes after the execution of command

	//Note 1: (In the original code, ) PasteCommand first checks a condition before 
	//executing this notification; In this aspect version, the call is
    //executed anyway (for the sake of the pointcut clarity ... generic), 
    //although the call is meaningful only if the condition in PasteCommand holds
	//Note 2: Redo/UndoCommand calls the notification for a command in the redo/undo stack, not for itself => 
	//exclude it from this general rule.
	//Note 3: (The original) ZoomCommand seems to be poorly implemented - it does not call
	//the notification, so I do the same in the aspect solution
    pointcut commandExecuteNotifyView(AbstractCommand acommand) :
    	commandExecuteCheckView(acommand)
    	&& !this(org.jhotdraw.util.UndoCommand)
    	&& !this(org.jhotdraw.util.RedoCommand)
		&& !this(org.jhotdraw.standard.CopyCommand)
		&& !this(org.jhotdraw.standard.ToggleGridCommand)
		&& !this(org.jhotdraw.contrib.zoom.ZoomCommand);

    //notify views to check "damages" after the command execution
	after(AbstractCommand acommand) returning : commandExecuteNotifyView(acommand) {
		acommand.view().checkDamage();
	}
}
