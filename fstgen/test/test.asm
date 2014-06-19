/** FLASH Cache Coherence Protocol dalla tesi di George Ma */
//modificato per poter essere tradotto in NuSMV
//aggiunte funzioni per gestire gli undef
//N: number of nodes
//L: numers of lines
//Q: the size of the message queue

asm flashProtocol_modificato_old
import ../../../STDL/StandardLibrary

signature:
	enum domain MsgType = {NO_MESS | GET_MSG | GETX_MSG | INV_MSG | WB_MSG |
							RPL_MSG | FWD_ACK_MSG | SWB_MSG | INV_ACK_MSG |
							NACK_MSG | NACKC_MSG | FWD_GET_MSG | FWD_GETX_MSG |
							PUT_MSG | PUTX_MSG | NACK_C2_MSG | PUT_SWB_MSG |
							PUTX_FWD_ACK_MSG}
	enum domain CcType = { CCGET| CCGETX| CCRPL| CCWB }
	domain Line subsetof Integer /* enum domain Line = {l1} */
	enum domain Phase = {READY | WAIT | INVALID_PHASE}
	enum domain State = { EXCLUSIVE | SHARED | INVALID}

	controlled messInTr: Agent -> MsgType
	controlled senderInTr: Agent -> Agent
	controlled sourceInTr: Agent -> Agent
	controlled senderInTrR: Agent -> Agent
	controlled sourceInTrR: Agent -> Agent
	controlled messInTrR: Agent -> MsgType
	controlled inSender: Agent -> Agent
	controlled inSource: Agent -> Agent
	controlled inMess: Agent -> MsgType
	controlled curPhase: Prod(Agent, Line) -> Phase
	controlled ccState: Prod(Agent, Line) -> State
	controlled pending: Line -> Boolean
	controlled owner: Line -> Agent
	controlled ownerIsUndef: Line -> Boolean //aggiunto
	controlled sharer: Prod(Line, Agent) -> Boolean

	monitored produceCCType : Agent -> CcType

	static home : Line -> Agent
	static lineInTr: Agent -> Line
	static lineInTrR : Agent -> Line
	static inLine : Agent -> Line

	static l1: Line
	static a1: Agent
	static a2: Agent
	static e: Agent

definitions:
	domain Line = {1}
	function l1 = 1
	function home($l in Line) = at({1 -> a1}, $l)
	//function messInTr($a in Agent) = at({a1 -> NO_MESS, a2 -> NO_MESS}, $a)
	function inLine($a in Agent) = at({a1 -> 11, a2 -> 11}, $a)
	function lineInTrR($a in Agent) = at({ a1 -> 11 , a2 -> 11}, $a)

	rule r_AppendToTransit($agentU in Agent, $senderU in Agent, $messU in MsgType, $sourceU in Agent, $lineU in Line) =
		if(messInTr($agentU)=NO_MESS) then
			par
				senderInTr($agentU) := $senderU
				messInTr($agentU) := $messU
				sourceInTr($agentU) := $sourceU
			endpar
		endif

	rule r_AppendRequestToTransit($agentU in Agent, $senderU in Agent, $messU in MsgType, $sourceU in Agent, $lineU in Line) =
		if(messInTrR($agentU) = NO_MESS) then
			par
				senderInTrR($agentU) := $senderU
				messInTrR($agentU) := $messU
				sourceInTrR($agentU) := $sourceU
			endpar
		endif

	rule r_R1UR2UR3UR4 =
		if(messInTrR(a1) = NO_MESS) then
			par
				if (produceCCType(self)=CCGET) and (curPhase(self,l1)=READY) then
					r_AppendRequestToTransit[home(l1), self, GET_MSG, self, l1]
				endif
				if (produceCCType(self)=CCGETX) and (curPhase(self,l1)=READY) then
					r_AppendRequestToTransit[home(l1), self, GETX_MSG, self, l1]
				endif
				if ((produceCCType(self)=CCRPL) and (curPhase (self, 11)=READY) and (ccState(self, 11)=SHARED)) then
					r_AppendRequestToTransit[home(l1), self, RPL_MSG, self, l1]
				endif
				if ((produceCCType(self)=CCWB) and (curPhase(self, l1)=READY) and (ccState (self,l1)= EXCLUSIVE)) then
					r_AppendRequestToTransit[home(11), self, WB_MSG, self, l1]
				endif
			endpar
		endif

	rule r_R5 =
		if((inMess(self)=GET_MSG) and (home(inLine(self))=self)) then
			if(pending(inLine(self))) then
				if(messInTr(inSource(self)) = NO_MESS) then
					par
						r_AppendToTransit[inSource(self), self, NACK_MSG, inSource(self), inLine(self)]
						inMess(self) := NO_MESS
					endpar
				endif
			else
				//if(owner(inLine(self)) != undef) then
				if(ownerIsUndef(inLine(self)) != true) then
					if(messInTr(owner(inLine(self)))=NO_MESS) then
						par
							r_AppendToTransit[owner(inLine(self)), self, FWD_GET_MSG, inSource(self), inLine(self)]
							pending(inLine(self)) := true
							inMess(self) := NO_MESS
						endpar
					endif
				else
					if(messInTr(inSource(self)) = NO_MESS) then
						par
							r_AppendToTransit[inSource(self), self, PUT_MSG, inSource(self) ,inLine(self)]
							inMess(self) := NO_MESS
							sharer(inLine(self), inSource(self)) := true
						endpar
					endif
				endif
			endif
		endif

	rule r_R6 =
		if(inMess(self) = FWD_GET_MSG) then
			if(ccState(self, inLine(self)) = EXCLUSIVE) then
				if(home(inLine(self))=inSource(self)) then
					if(messInTr(home(inLine(self))) = NO_MESS) then
						par
							r_AppendToTransit[home(inLine(self)), self, PUT_SWB_MSG, inSource(self), inLine(self)]
							ccState(self, inLine(self)) := SHARED
							inMess(self) := NO_MESS
						endpar
					endif
				else
					if((messInTr(inSource(self)) = NO_MESS) and (messInTr(home(inLine(self))) = NO_MESS)) then
						par
							r_AppendToTransit[inSource(self), self, PUT_MSG, inSource(self), inLine(self)]
							r_AppendToTransit[home(inLine(self)), self, SWB_MSG, inSource(self), inLine(self)]
							ccState(self, inLine(self)) := SHARED
							inMess(self) := NO_MESS
						endpar
					endif
				endif
			else
				if(home(inLine(self)) = inSource(self)) then
					if(messInTr(home(inLine(self))) = NO_MESS) then
						par
							r_AppendToTransit[home(inLine(self)), self, NACK_C2_MSG, inSource(self), inLine(self)]
							inMess(self) := NO_MESS
			 			endpar
		 			endif
		 		else
		 			if((messInTr(inSource(self)) = NO_MESS) and (messInTr(home(inLine(self)))= NO_MESS)) then
						par
							r_AppendToTransit[inSource(self), self, NACK_MSG, inSource(self), inLine(self)]
							r_AppendToTransit[home(inLine(self)), self, NACKC_MSG, inSource(self), inLine(self)]
							inMess(self) := NO_MESS
						endpar
					endif
				endif
			endif
		endif

	rule r_R7 =
		if(inMess(self) = PUT_MSG) then
			par
				if(curPhase(self, inLine(self)) != INVALID_PHASE) then
					ccState(self, inLine(self)) := SHARED
				endif
				curPhase(self, inLine(self)) := READY
				inMess(self) := NO_MESS
			endpar
		endif

	rule r_R8 =
		if((inMess(self)=SWB_MSG) and (home(inLine(self))=self)) then
			par
				sharer(inLine(self), inSource(self)) := true
				//if(owner(inLine(self)) != undef) then
				if(ownerIsUndef(inLine(self)) != true) then
					sharer(inLine(self), owner(inLine(self))) := true
				endif
				//owner(inLine(self)) := undef
				ownerIsUndef(inLine(self)) := true
				pending(inLine(self)) := false
				inMess(self) := NO_MESS
			endpar
		endif

	rule r_R7UR8 =
		if(inMess(self) = PUT_SWB_MSG) then
			par
				if curPhase(self, inLine(self)) != INVALID_PHASE then
					ccState(self, inLine(self)) := SHARED
				endif
				curPhase(self, inLine(self)) := READY
				sharer(inLine(self), inSource(self)) := true
				//if(owner(inLine(self)) != undef) then
				if(ownerIsUndef(inLine(self)) != true) then
					sharer(inLine(self), owner(inLine(self))) := true
				endif
				//owner(inLine(self)) := undef
				ownerIsUndef(inLine(self)) := true
				pending(inLine(self)) := false
				inMess(self) := NO_MESS
			endpar
		endif

	rule r_R9 =
		if(inMess(self) = NACK_MSG) then
			par
				curPhase(self, inLine(self)) := READY
				inMess(self) := NO_MESS
			endpar
		endif

	rule r_R10 =
		if ((inMess(self)=NACKC_MSG) and (home(inLine(self))=self)) then
			par
				pending(inLine(self)) := false
				inMess(self) := NO_MESS
			endpar
		endif

	rule r_R9UR10 =
		if(inMess(self) = NACK_C2_MSG) then
			par
				curPhase(self, inLine(self)) := READY
				pending(inLine(self)) := false
				inMess(self) := NO_MESS
			endpar
		endif

	rule r_R11 =
		if((inMess(self)=GETX_MSG) and (home(inLine(self))=self)) then
			if(pending(inLine(self))=true) then
				if(messInTr(inSource(self))=NO_MESS) then
					par
						r_AppendToTransit[inSource(self), self, NACK_MSG, inSource(self), inLine(self)]
						inMess(self) := NO_MESS
					endpar
				endif
			else
				//if(owner(inLine(self)) != undef) then
				if(ownerIsUndef(inLine(self)) != true) then
					if(messInTr(owner(inLine(self)))=NO_MESS) then
						par
							r_AppendToTransit[owner(inLine(self)), self, FWD_GETX_MSG, inSource(self), inLine(self)]
							pending(inLine(self)) := true
							inMess(self) := NO_MESS
						endpar
					endif
				else
					//if(forall $agentU in {a1, a2} holds (not(sharer(inLine(self), agentU)))) then
					if( forall $agentU in Agent with (not(sharer(inLine(self), $agentU)))) then
						if(messInTr(inSource(self)) = NO_MESS) then
							par
								r_AppendToTransit[inSource(self), self, PUTX_MSG, inSource(self), inLine(self)]
								owner(inLine(self)) := inSource(self)
								ownerIsUndef(inLine(self)) := false
								inMess(self) := NO_MESS
							endpar
						endif
					else
						//if(forall agentU in {al, a2} holds (not(sharer(inLine(self), agentU)) or (messInTr(agentU)=NO_MESS))) then
						if(forall $agentU2 in Agent with (not(sharer(inLine(self), $agentU2)) or (messInTr($agentU)=NO_MESS))) then
							par
								forall $agentU3 in Agent with (sharer(inLine(self), $agentU3)) do
									par
										r_AppendToTransit[$agentU3, self, INV_MSG, inSource(self), inLine(self)]
										pending(inLine(self)) := true
									endpar
								inMess(self) := NO_MESS
							endpar
						endif
					endif
				endif
			endif
		endif

	rule r_R12 =
		if(inMess(self) = FWD_GETX_MSG) then
			if(ccState(self, inLine(self)) = EXCLUSIVE) then
				if(home(inLine(self))=inSource(self)) then
					if(messInTr(home(inLine(self))) = NO_MESS) then
						par
							r_AppendToTransit[home(inLine(self)), self, PUTX_FWD_ACK_MSG, inSource(self), inLine(self)]
							ccState(self, inLine(self)) := INVALID
							inMess(self) := NO_MESS
						endpar
					endif
				else
					if (messInTr(inSource(self)) = NO_MESS) and (messInTr(home(inLine(self))) = NO_MESS) then
						par
							r_AppendToTransit[inSource(self), self, PUTX_MSG, inSource(self), inLine(self)]
							r_AppendToTransit[home(inLine(self)), self, FWD_ACK_MSG, inSource(self), inLine(self)]
							ccState(self, inLine(self)) := INVALID
							inMess(self) := NO_MESS
						endpar
					endif
				endif
			else
				if(home(inLine(self))=inSource(self)) then
					if (messInTr(home(inLine(self))) = NO_MESS) then
						par
							r_AppendToTransit[inSource(self), self, NACK_C2_MSG, inSource(self), inLine(self)]
							inMess(self) := NO_MESS
						endpar
					endif
				else
					if((messInTr(inSource(self))= NO_MESS) and (messInTr(home(inLine(self))) = NO_MESS)) then
						par
							r_AppendToTransit[inSource(self), self, NACK_MSG, inSource(self), inLine(self)]
							r_AppendToTransit[home(inLine(self)), self, NACKC_MSG, inSource(self), inLine(self)]
							inMess(self) := NO_MESS
						endpar
					endif
				endif
			endif
		endif

	rule r_R13 =
		if (inMess(self) = PUTX_MSG) then
			par
				ccState(self, inLine(self)) := EXCLUSIVE
				curPhase(self, inLine(self)) := READY
				inMess(self) := NO_MESS
			endpar
		endif

	rule r_R14 =
		if((inMess(self) = FWD_ACK_MSG) and (home(inLine(self)) = self)) then
			par
				owner(inLine(self)) := inSource(self)
				ownerIsUndef(inLine(self)) := false
				pending(inLine(self)) := false
				inMess(self) := NO_MESS
			endpar
		endif

	rule r_R13UR14 =
		if(inMess(self) = PUTX_FWD_ACK_MSG)then
			par
				ccState(self, inLine(self)) := EXCLUSIVE
				curPhase(self, inLine(self)) := READY
				owner(inLine(self)) := inSource(self)
				ownerIsUndef(inLine(self)) := false
				pending(inLine(self)) := false
				inMess(self) := NO_MESS
			endpar
		endif

	rule r_R15 =
		if(inMess(self)=INV_MSG) then
			if(messInTr(home(inLine(self)))=NO_MESS) then
				par
					r_AppendToTransit[home(inLine(self)), self, INV_ACK_MSG, inSource(self), inLine(self)]
					inMess(self) := NO_MESS
					if(ccState(self, inLine(self))=SHARED) then
						ccState(self, inLine(self)) := INVALID
					else
						if(curPhase(self, inLine(self))=WAIT) then
							curPhase(self, inLine(self)) := INVALID_PHASE
						endif
					endif
				endpar
			endif
		endif

	rule r_R16 =
		if((inMess(self)=INV_ACK_MSG) and (home(inLine(self))=self)) then
			forall $agentU in {a1, a2} do
				if(inSender(self)=$agentU) then
					par
						sharer(inLine(self), $agentU) := false
						if(forall $otherUagentU in {a1, a2} with ($otherUagentU=$agentU or sharer(inLine(self), $otherUagentU)=false)) then
							if(messInTr(inSource(self)) = NO_MESS) then
								par
									r_AppendToTransit[inSource(self), self, PUTX_MSG, inSource(self), inLine(self)]
									pending(inLine(self)) := false
									inMess(self) := NO_MESS
								endpar
							endif
						else
							inMess(self) := NO_MESS
						endif
					endpar
				endif
			endif

	rule r_R17 =
		if((inMess(self)=RPL_MSG) and (home(inLine(self))=self)) then
			par
				if((sharer(inLine(self), inSender(self)) = true) and (pending(inLine(self))= false)) then
					par
						sharer(inLine(self), inSender(self)) := false
						ccState(self,inLine(self)) := INVALID
					endpar
				endif
				inMess(self) := NO_MESS
			endpar
		endif

	rule r_R18 =
		if((inMess(self)=WB_MSG) and (home(inLine(self))=self)) then
			par
				//if(owner(inLine(self))!= undef) then
				if(ownerIsUndef(inLine(self))!= true) then
					par
						//owner(inLine(self)) := undef
						ownerIsUndef(inLine(self)) := true
						ccState(self, inLine(self)) := INVALID
					endpar
				endif
				inMess(self) := NO_MESS
			endpar
		endif

	rule r_behavior =
		par
			r_R1UR2UR3UR4[]
			r_R5[]
			r_R6[]
			r_R7[]
			r_R8[]
			r_R7UR8[]
			r_R9[]
			r_R10[]
			r_R9UR10[]
			r_R11[]
			r_R12[]
			r_R13[]
			r_R14[]
			r_R13UR14[]
			r_R15[]
			r_R16[]
			r_R17[]
			r_R18[]
		endpar

	rule r_ClearMessInTr($agentU in Agent) =
		messInTr($agentU) := NO_MESS

	rule r_SendMess($agentU in Agent) =
		par
			inSender($agentU) := senderInTr($agentU)
			inMess($agentU) := messInTr($agentU)
			inSource($agentU) := sourceInTr($agentU)
			r_ClearMessInTr[$agentU]
		endpar

	rule r_SendR($agentU in Agent) =
		par
			inSender($agentU) := senderInTrR($agentU)
			inMess($agentU) := messInTrR($agentU)
			inSource($agentU) := sourceInTrR($agentU)
			messInTrR($agentU) := NO_MESS
		endpar

	rule r_SendRequest($agentU in Agent) =
		if((messInTrR($agentU)=GET_MSG) and (curPhase(senderInTrR($agentU), lineInTrR($agentU))=READY) and (ccState(senderInTrR($agentU), lineInTrR($agentU))=INVALID)) then
			par
				r_SendR[$agentU]
				curPhase(senderInTrR($agentU), lineInTrR($agentU)) := WAIT
			endpar
		else
			if((messInTrR($agentU)=GETX_MSG) and (curPhase(senderInTrR($agentU), lineInTrR($agentU))=READY)) then
				par
					r_SendR[$agentU]
					curPhase(senderInTrR($agentU), lineInTrR($agentU)) := WAIT
				endpar
			else
				if((messInTrR($agentU)=RPL_MSG) and (curPhase(senderInTrR($agentU), lineInTrR($agentU))=READY) and (ccState(senderInTrR($agentU), lineInTrR($agentU))=SHARED)) then
					par
						r_SendR[$agentU]
						ccState(senderInTrR($agentU), lineInTr($agentU)) := INVALID
					endpar
				else
					if((messInTrR($agentU) = WB_MSG) and (curPhase(senderInTrR($agentU), lineInTrR($agentU)) = READY) and (ccState(senderInTrR($agentU), lineInTrR($agentU))=EXCLUSIVE)) then
						par
							r_SendR[$agentU]
							ccState(senderInTrR($agentU), lineInTrR($agentU)) := INVALID
						endpar
					endif
				endif
			endif
		endif

	//rule r_env =
	rule r_env =
		forall $a in {a1, a2} do
			if(inMess($a) = NO_MESS) then
				if(messInTr($a) != NO_MESS) then
					r_SendMess[$a]
				else
					if((messInTrR($a) != NO_MESS) and (inMess($a)=NO_MESS)) then
						r_SendRequest[$a]
					endif
				endif
			endif

	/*rule r_Skip =
		skip*/

	//mio
	rule r_agentRule =
		if(self=e) then
			r_env[]
		else
			r_behavior[]
		endif


	//mio
	main rule r_main =
		par
			program(e)
			program(a1)
			program(a2)
		endpar


default init s0:
	function messInTr($a in Agent) = at({ a1 -> NO_MESS, a2 -> NO_MESS}, $a)
	function senderInTr($a in Agent) = at({ a1 -> a1 , a2 -> a1 }, $a)
	function sourceInTr($a in Agent) = at({ a1 -> a1 , a2 -> a1}, $a)
	function senderInTrR($a in Agent ) = at({ a1 -> a2 , a2 -> a2}, $a)
	function sourceInTrR($a in Agent) = at({ a1 -> a1 , a2 -> a1}, $a)
	function messInTrR($a in Agent) = at({a1 -> NO_MESS, a2 -> NO_MESS}, $a)
	function inSender($a in Agent) = at({a1 -> a2 , a2 -> a2}, $a)
	function inSource($a in Agent) = at({a1 -> a2, a2 -> a2}, $a)
	function inMess($a in Agent) = at({ a1 -> NO_MESS, a2 -> NO_MESS}, $a)
	function curPhase($a in Agent, $l in Line) = at({(a1, l1) -> READY, (a2, l1) -> READY}, ($a, $l))
	function ccState($a in Agent, $l in Line) = at({(a1, l1) -> INVALID, (a2,l1) -> INVALID}, ($a, $l))
	function pending($l in Line) = at({l1 -> false}, $l)
	function sharer($l in Line, $a in Agent) = at({(l1, a1) -> false, (l1, a2) -> false}, ($l, $a))

	agent Agent:
		r_agentRule[]
