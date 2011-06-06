



package com.lowagie.toolbox.swing;

import javax.swing.SwingUtilities;


public abstract class EventDispatchingThread {

    
    private static class ThreadWrapper {
        private Thread thread;
        ThreadWrapper(Thread t) { thread = t; }
        synchronized Thread get() { return thread; }
        synchronized void clear() { thread = null; }
    }

    
    private Object value;
    
    private ThreadWrapper thread;

    
    public EventDispatchingThread() {
        final Runnable doFinished = new Runnable() {
           public void run() { finished(); }
        };

        Runnable doConstruct = new Runnable() {
            public void run() {
                try {
                    value = construct();
                }
                finally {
                    thread.clear();
                }
                SwingUtilities.invokeLater(doFinished);
            }
        };
        Thread t = new Thread(doConstruct);
        thread = new ThreadWrapper(t);
    }

    
    public abstract Object construct();

    
    public void start() {
        Thread t = thread.get();
        if (t != null) {
            t.start();
        }
    }

    
    public void interrupt() {
        Thread t = thread.get();
        if (t != null) {
            t.interrupt();
        }
        thread.clear();
    }

    
    public void finished() {
    }

    
    public Object get() {
        while (true) {
            Thread t = thread.get();
            if (t == null) {
                return value;
            }
            try {
                t.join();
            }
            catch (InterruptedException e) {
                Thread.currentThread().interrupt(); 
                return null;
            }
        }
    }
}
