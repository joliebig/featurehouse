package net.sf.jabref;

import spin.Spin;


public abstract class AbstractWorker implements Worker, CallBack {

    private Worker worker;
    private CallBack callBack;

    public AbstractWorker() {
	worker = (Worker)Spin.off(this);
	callBack = (CallBack)Spin.over(this);

    }

    public void init() throws Throwable {

    }

    
    public Worker getWorker() {
	return worker;
    }

    
    public CallBack getCallBack() {
	return callBack;
    }

    
    public void update() {
    }
}
