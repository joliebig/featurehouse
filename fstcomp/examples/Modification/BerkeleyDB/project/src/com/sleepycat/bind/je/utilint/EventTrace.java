package com.sleepycat.je.utilint; 
import de.ovgu.cide.jakutil.*; 
public  class  EventTrace {
	 static private int MAX_EVENTS=100;

	 static public final boolean TRACE_EVENTS=false;

	 static int currentEvent=0;

	 static final EventTrace[] events=new EventTrace[MAX_EVENTS];

	 static final int[] threadIdHashes=new int[MAX_EVENTS];

	 static boolean disableEvents=false;

	 protected String comment;

	 public EventTrace( String comment){ this.comment=comment; }

	 public EventTrace(){ comment=null; }

	
static public  class  ExceptionEventTrace  extends EventTrace {
		 private Exception event;

		 public ExceptionEventTrace(){ event=new Exception(); }

		 public String toString__wrappee__base(){ return Tracer.getStackTrace(event); }

		 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }


	}

	 public String toString__wrappee__base(){ return comment; }

	 public String toString(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	toString__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 static public void addEvent__wrappee__base( EventTrace event){ if (disableEvents) { return; } int nextEventIdx=currentEvent++ % MAX_EVENTS; events[nextEventIdx]=event; threadIdHashes[nextEventIdx]=System.identityHashCode(Thread.currentThread()); }

	 static public void addEvent( EventTrace event){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addEvent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 static public void addEvent__wrappee__base( String comment){ if (disableEvents) { return; } addEvent(new EventTrace(comment)); }

	 static public void addEvent( String comment){ t.in(Thread.currentThread().getStackTrace()[1].toString());	addEvent__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 static public void dumpEvents__wrappee__base(){ if (disableEvents) { return; } System.out.println("----- Event Dump -----"); EventTrace[] oldEvents=events; int[] oldThreadIdHashes=threadIdHashes; disableEvents=true; int j=0; for (int i=currentEvent; j < MAX_EVENTS; i++) { EventTrace ev=oldEvents[i % MAX_EVENTS]; if (ev != null) { int thisEventIdx=i % MAX_EVENTS; System.out.print(oldThreadIdHashes[thisEventIdx] + " "); System.out.println(j + "(" + thisEventIdx+ "): "+ ev); } j++; } }

	 static public void dumpEvents(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	dumpEvents__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
