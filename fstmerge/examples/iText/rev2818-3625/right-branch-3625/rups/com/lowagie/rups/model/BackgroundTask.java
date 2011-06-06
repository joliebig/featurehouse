

package com.lowagie.rups.model;

import javax.swing.SwingUtilities;



public abstract class BackgroundTask {

    
    private static class ThreadWrapper {
        private Thread thread;
        ThreadWrapper(Thread t) { thread = t; }
        synchronized Thread get() { return thread; }
        synchronized void clear() { thread = null; }
    }

    
    private ThreadWrapper thread;

    
    public BackgroundTask() {
        final Runnable doFinished = new Runnable() {
           public void run() { finished(); }
        };

        Runnable doConstruct = new Runnable() {
            public void run() {
                try {
                    doTask();
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

    
    public abstract void doTask();

    
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
}
