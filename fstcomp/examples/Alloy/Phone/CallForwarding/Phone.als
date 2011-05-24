module Phone

sig Phone {
	forward: lone Phone
} 

pred incomingCall [inCall: Call, disj phone, phone': Phone] {
	phone.currentState = Busy => 	phone'.currentState = Busy && 
									phone'.currentCall = phone.currentCall &&
									phone'.currentCall != inCall &&
									no phone.forward &&
									(one phone'': Phone | 	phone'' != phone' && 
															phone'' != phone && 
															phone''.currentState = Busy && 
															phone''.currentCall = inCall &&
															no phone''.forward &&
															phone'.forward = phone'' &&
															(no phone''': Phone | 	phone''' != phone'' && 
																					phone''' != phone' && 
																					phone''' != phone))
	phone.currentState =Idle => (phone'.currentState = Busy &&
								 phone'.currentCall = inCall && 
								 no phone'.forward &&
								 no phone.forward &&
								 (no phone'': Phone | phone'' != phone' && phone'' != phone))
}

assert isForwarded {
	all disj phone, phone': Phone | all inCall: Call | (incomingCall [inCall, phone, phone']) => ((phone.currentState = Idle <=> no phone'.forward) && (phone.currentState = Busy <=> one phone'.forward))
}

check isForwarded for 5