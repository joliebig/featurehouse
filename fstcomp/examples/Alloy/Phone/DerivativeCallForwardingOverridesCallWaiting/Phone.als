module Phone

fact disableWaiting {
	Suspended not in Phone.currentState
	all phone: Phone | (phone.currentState = Idle || phone.currentState = Busy) => no phone.waitingCall
}