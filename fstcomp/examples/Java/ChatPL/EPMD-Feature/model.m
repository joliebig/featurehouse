//NoAbstractFeatures

Chat_ : Chat+ :: _Chat ;

Chat : Encryption+ :: Encryption_
	| Color
	| Authentication
	| Logging ;

Encryption : Rot13
	| Reverse ;

