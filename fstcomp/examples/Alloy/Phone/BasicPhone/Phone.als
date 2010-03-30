module Phone

sig Phone {
	currentState: one State,
	currentCall: lone Call
} 

fact noEmptyCalls {
	no call: Call | call not in Phone.currentCall
}

sig Call {} 

abstract sig State {}

fact states {
	all phone: Phone | phone.currentState = Busy => one phone.currentCall 
	all phone: Phone | phone.currentState = Idle => no phone.currentCall
}

one sig Idle extends State {} 
one sig Busy extends State {} 


pred incomingCall [inCall: Call, disj phone, phone': Phone] {
	phone.currentState = Busy => phone'.currentState = Busy && phone'.currentCall = phone.currentCall
	phone.currentState =Idle => phone'.currentState = Busy && phone'.currentCall = inCall
	no phone'': Phone | phone'' != phone' && phone'' != phone
}

run incomingCall for 5