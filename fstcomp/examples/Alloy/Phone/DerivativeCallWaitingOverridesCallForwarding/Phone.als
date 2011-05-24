module Phone

fact disableForwarding {
	all phone: Phone | no phone.forward
}