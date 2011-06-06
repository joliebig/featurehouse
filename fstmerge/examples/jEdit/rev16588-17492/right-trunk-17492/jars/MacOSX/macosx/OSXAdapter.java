

package macosx;


import java.util.*;
import java.io.File;
import java.awt.event.*;
import javax.swing.*;

import java.lang.reflect.*;

import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.browser.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.options.GlobalOptions;
import org.gjt.sp.util.Log;


public class OSXAdapter implements InvocationHandler
{
	protected Object targetObject;
	protected Method targetMethod;
	protected String proxySignature;
	
	static Object macOSXApplication;
	
	
	
	protected OSXAdapter(String proxySignature, Object target, Method handler) {
		this.proxySignature = proxySignature;
		this.targetObject = target;
		this.targetMethod = handler;
	}
	
	
	
	public static void setQuitHandler(Object target, Method quitHandler) {
		setHandler(new OSXAdapter("handleQuit", target, quitHandler));
	}
	
	
	
	public static void setAboutHandler(Object target, Method aboutHandler) {
		boolean enableAboutMenu = (target != null && aboutHandler != null);
		if (enableAboutMenu) {
			setHandler(new OSXAdapter("handleAbout", target, aboutHandler));
		}
		
		
		try {
			Method enableAboutMethod = macOSXApplication.getClass().getDeclaredMethod("setEnabledAboutMenu", new Class[] { boolean.class });
			enableAboutMethod.invoke(macOSXApplication, new Object[] { Boolean.valueOf(enableAboutMenu) });
		} catch (Exception ex) {
			System.err.println("OSXAdapter could not access the About Menu");
			ex.printStackTrace();
		}
	}
	
	
	
	public static void setPreferencesHandler(Object target, Method prefsHandler) {
		boolean enablePrefsMenu = (target != null && prefsHandler != null);
		if (enablePrefsMenu) {
			setHandler(new OSXAdapter("handlePreferences", target, prefsHandler));
		}
		
		
		try {
			Method enablePrefsMethod = macOSXApplication.getClass().getDeclaredMethod("setEnabledPreferencesMenu", new Class[] { boolean.class });
			enablePrefsMethod.invoke(macOSXApplication, new Object[] { Boolean.valueOf(enablePrefsMenu) });
		} catch (Exception ex) {
			System.err.println("OSXAdapter could not access the About Menu");
			ex.printStackTrace();
		}
	}
	
	
	
	
	public static void setFileHandler(Object target, Method fileHandler) {
		setHandler(new OSXAdapter("handleOpenFile", target, fileHandler) {
			
			
			public boolean callTarget(Object appleEvent) {
				if (appleEvent != null) {
					try {
						Method getFilenameMethod = appleEvent.getClass().getDeclaredMethod("getFilename", (Class[])null);
						String filename = (String) getFilenameMethod.invoke(appleEvent, (Object[])null);
						this.targetMethod.invoke(this.targetObject, new Object[] { filename });
					} catch (Exception ex) {
						
					}
				}
				return true;
			}
		});
	}
	
	public static void setReOpenApplicationHandler(Object target, Method appHandler) {
		setHandler(new OSXAdapter("handleReOpenApplication", target, appHandler));
	}
	
	
	public static void setHandler(OSXAdapter adapter) {
		try {
			Class applicationClass = Class.forName("com.apple.eawt.Application");
			if (macOSXApplication == null) {
				macOSXApplication = applicationClass.getConstructor((Class[])null).newInstance((Object[])null);
			}
			Class applicationListenerClass = Class.forName("com.apple.eawt.ApplicationListener");
			Method addListenerMethod = applicationClass.getDeclaredMethod("addApplicationListener", new Class[] { applicationListenerClass });
			
			Object osxAdapterProxy = Proxy.newProxyInstance(OSXAdapter.class.getClassLoader(), new Class[] { applicationListenerClass }, adapter);
			addListenerMethod.invoke(macOSXApplication, new Object[] { osxAdapterProxy });
		} catch (ClassNotFoundException cnfe) {
			System.err.println("This version of Mac OS X does not support the Apple EAWT.  ApplicationEvent handling has been disabled (" + cnfe + ")");
		} catch (Exception ex) {  
			System.err.println("Mac OS X Adapter could not talk to EAWT:");
			ex.printStackTrace();
		}
	}
	
	
	
	
	public boolean callTarget(Object appleEvent) throws InvocationTargetException, IllegalAccessException {
		Object result = targetMethod.invoke(targetObject, (Object[])null);
		if (result == null) {
			return true;
		}
		return Boolean.valueOf(result.toString()).booleanValue();
	}
	
	
	
	public Object invoke (Object proxy, Method method, Object[] args) throws Throwable {
		if (isCorrectMethod(method, args)) {
			boolean handled = callTarget(args[0]);
			setApplicationEventHandled(args[0], handled);
		}
		
		return null;
	}
	
	
	
	protected boolean isCorrectMethod(Method method, Object[] args) {
		return (targetMethod != null && proxySignature.equals(method.getName()) && args.length == 1);
	}
	
	
	
	protected void setApplicationEventHandled(Object event, boolean handled) {
		if (event != null) {
			try {
				Method setHandledMethod = event.getClass().getDeclaredMethod("setHandled", new Class[] { boolean.class });
				
				setHandledMethod.invoke(event, new Object[] { Boolean.valueOf(handled) });
			} catch (Exception ex) {
				System.err.println("OSXAdapter was unable to handle an ApplicationEvent: " + event);
				ex.printStackTrace();
			}
		}
	}
}
