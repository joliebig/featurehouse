module Phone

sig Phone {
	waitingCall: set Call
} 

fact noEmptyCalls {
	no call: Call | call not in Phone.(currentCall + waitingCall)
}

fact states {
	all phone: Phone | phone.currentState = Busy => one phone.currentCall 
	all phone: Phone | phone.currentState = Idle => no phone.currentCall
	all phone: Phone | phone.currentState = Idle => no phone.waitingCall
	all phone: Phone | phone.currentState = Suspended => one phone.currentCall 
	all phone: Phone | phone.currentState = Suspended => some phone.waitingCall 
	no (Phone.currentCall & Phone.waitingCall)
}

one sig Suspended extends State {} 

pred incomingCall [inCall: Call, disj phone, phone': Phone] {
	phone.currentState = Idle => 	phone'.currentState = Busy && 
									phone'.currentCall = inCall  && 
									no phone'.waitingCall && 
									no phone.waitingCall
	phone.currentState = Busy => 	phone'.currentState = Suspended && 
									phone'.currentCall = phone.currentCall  && 
									phone'.currentCall != inCall &&
									phone'.waitingCall = inCall && 
									no phone.waitingCall
	phone.currentState = Suspended => 	phone'.currentState = Suspended && 
										phone'.currentCall = phone.currentCall  && 
										phone'.currentCall != inCall &&
										inCall not in phone.waitingCall &&
										phone'.waitingCall = phone.waitingCall + inCall
	no phone'': Phone | phone'' != phone' && phone'' != phone
}

assert isSuspended {
	all disj phone, phone': Phone | all inCall: Call | (incomingCall [inCall, phone, phone']) => ((phone.currentState = Idle <=> no phone'.waitingCall) && (phone.currentState = Busy <=> one phone'.waitingCall) && (phone.currentState = Suspended <=> (some phone.waitingCall && some phone'.waitingCall)))
}

check isSuspended for 5

