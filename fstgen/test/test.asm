//DANIELA CUGLIETTA, PATRIZIA SCANDURRA, ANGELO
//EXERCISE pag. 188			SU UNICO FILE
// rifatto in modo di avere le funzioni da agenti al loro dominio

asm flashProtocol

import ../../StandardLibrary

//declare universes and functions


signature:

    enum domain Phasetransport = {NORMALRUN | CRITICALRUN | STOPPED | STOPPEDINLOADPOS}
	enum domain Phasert = {STOPPEDINLOADPOS2| STOPPEDINUNLOADPOS | 	MOVINGTOLOADPOS | 			MOVINGTOUNLOADPOS}
	enum domain Phaserobot = {WAITINGINLOADDEPBELTPOS | WAITINGINUNLOADPRESSPOS |
	WAITINGINUNLOADTABLEPOS | WAITINGINLOADPRESSPOS |UNLOADINGTABLE|UNLOADINGPRESS| 	LOADINGDEPBELT| LOADINGPRESS| MOVINGTOUNLOADPRESSPOS|MOVINGTOLOADDEPBELTPOS| 	MOVINGTOLOADPRESSPOS| MOVINGTOUNLOADTABLEPOS| STOPPEDIULOADPOS| OPENFLOAD | 	OPENFUNLOAD}
	enum domain Phasepress= {OPENFORLOAD | OPENFORUNLOAD | MOVINGTOMIDDLEPOSITION | 			MOVINGTOTOPPOSITION | MOVINGTOBOTTOMPOSITION| CLOSEDFORFORGIN}
	enum domain Phasedeposit= {NORMAL| CRITICAL | STOP}
	enum domain Phasecrane = {WAITINGTOUNLOADDEPOSITBELT|UNLOADINGDEPOSITBELT| MOVINGTOLOADFEEDBELTPOS| WAITINGLOADFEEDBELT| LOADINGFEEDBELT |MOVINGTOUNLOADDEPOSITBELTPOS}

// Agent "TRANSPORT"
	domain TransportAgent subsetof Agent

// Agent "ERT"
	domain ERTAgent subsetof Agent

// Agent "ROBOT"
	domain ROBOTAgent subsetof Agent

// Agent "PRESS"
	domain PRESSAgent subsetof Agent

// Agent "DEPOSIT"
	domain DEPOSITAgent subsetof Agent

// Agent "CRANE"
	domain CRANEAgent subsetof Agent


//function for Agent "TRANSPORT"

	dynamic monitored piecefeedbeltlightbarrier : Boolean
	derived tablereadyforloading: TransportAgent -> Boolean
	static tableinloadposition: TransportAgent-> Boolean
	dynamic controlled currphaseTra:  TransportAgent -> Phasetransport

//function for Agent "ERT"

	dynamic monitored loadpositionreached: Boolean
	dynamic monitored unloadpositionreached: Boolean
	dynamic controlled currphasee: Agent -> Phasert

//fuction for Agent "ROBOT"
	dynamic controlled currphaser: Agent-> Phaserobot
	dynamic monitored unloadtablecomp: Boolean
	dynamic monitored unloadpresscomp: Boolean
	dynamic monitored loaddepbeltcomp: Boolean
	dynamic monitored loadpresscomp: Boolean
	dynamic monitored unloadpressposreached: Boolean
	dynamic monitored loaddepbeltposreached: Boolean
	dynamic monitored loadpressposreached: Boolean
	dynamic monitored unloadtableposreached: Boolean
	static tablereadyforunloading: ROBOTAgent -> Boolean
	static pressreadyforloading: ROBOTAgent ->Boolean
	static pressreadyforunloading: ROBOTAgent ->Boolean
	static pressinloadposition: ROBOTAgent ->Boolean
	static pressinunloadposition: ROBOTAgent -> Boolean
	static tableinunloadposition: ROBOTAgent -> Boolean

//function for Agent "PRESS"
	dynamic controlled currphasep : Agent -> Phasepress
	dynamic monitored middleposition :Boolean
	dynamic monitored bottomposition :Boolean
	dynamic monitored topposition :Boolean
	dynamic monitored forgincompleted :Boolean

//function for Agent "DEPOSIT"
	dynamic controlled currphased : Agent -> Phasedeposit
	dynamic monitored pieceindepositbeltlightbarrier: Boolean

//function for Agent "CRANE"
	dynamic controlled currphasec : Agent -> Phasecrane
	dynamic monitored unloadingdepositbeltcompleted: Boolean
	dynamic monitored loadfeedbeltposreached: Boolean
	dynamic monitored loadingdepositbeltcompleted: Boolean
	dynamic monitored unloaddepositbeltposreached: Boolean

//function for Agent "ROBOT", "TRANSPORT", "ERT"
	dynamic controlled/*shared*/ tableloaded: Boolean

//function for Agent "ROBOT", "PRESS"
	dynamic controlled/*shared*/ pressloaded: Boolean

//function for Agent "ROBOT", "DEPOSIT"
	dynamic controlled/*shared*/ depositbeltreadyforloading: Agent-> Boolean

//function for Agent "DEPOSIT", "CRANE"
	dynamic controlled/*shared*/ pieceatdepositbeltend: Boolean

//function for Agent "CRANE", "TRANSPORT"
	dynamic controlled/*shared*/ feedbeltfree: Boolean
// THE AGENTS

// Agent "TRANSPORT"
	static transport : TransportAgent

// Agent "ERT"
	static  ert : ERTAgent

// Agent "ROBOT"
	static robot : ROBOTAgent

// Agent "PRESS"
	static press : PRESSAgent

// Agent "DEPOSIT"
	static deposit : DEPOSITAgent

// Agent "CRANE"
	static crane : CRANEAgent




definitions:

//function for Agent "TRANSPORT"

//function tablereadyforloading

	function tablereadyforloading ($a in TransportAgent)=
			tableinloadposition($a) = true and
			tableloaded = false

//function tableinloadposition

	function tableinloadposition ($a in TransportAgent) =
		currphaseTra($a) = STOPPED

//functions for Agent "ROBOT"

//function tableinunloadposition

	function tableinunloadposition ($a in ROBOTAgent)=
					currphaser ($a) = STOPPEDIULOADPOS

//function tablereadyforunloading

	function tablereadyforunloading ($a in ROBOTAgent)=
			tableinunloadposition($a) = true and tableloaded = true

//function pressreadyforunloading

	function pressreadyforunloading ($a in ROBOTAgent)=
			pressinloadposition($a) = true and pressloaded = true

//function pressinunloadposition

	function pressinunloadposition ($a in ROBOTAgent)=
			currphaser($a)=OPENFUNLOAD

//function pressreadyforloading

	function pressreadyforloading ($a in ROBOTAgent)=
 			pressinloadposition($a)= true and pressloaded = false

//function pressinloadposition

	function pressinloadposition ($a in ROBOTAgent)=
			currphaser ($a)= OPENFLOAD


//Rules for Agent TRANSPORT

//Rule that defines the NormalRun

	rule r_FbNormal =
		if currphaseTra (self) = NORMALRUN and piecefeedbeltlightbarrier = true then
			feedbeltfree:=true
		endif

//Rule
	rule r_Change =
		if tablereadyforloading (self)=true then
			 currphaseTra (self):=CRITICALRUN
		else
			 currphaseTra (self):=STOPPED
		endif

//Rule that defines the Stopped

	rule r_FbStopped =
		if currphaseTra (self)= STOPPED and tablereadyforloading(self)=true then
			  currphaseTra (self) :=CRITICALRUN
		endif

//Rule that defines the CriticalRun

	rule r_FbCritical =
		if currphaseTra (self)= CRITICALRUN and piecefeedbeltlightbarrier= false then
			seq
			  currphaseTra (self):=NORMALRUN
			  tableloaded:= true
			endseq
		endif

//Rules for Agent "ERT"

//Rule that defines the WaitingLoad

	rule r_WaitingLoad =
		if currphasee (self)= STOPPEDINLOADPOS2 and tableloaded = true then
			currphasee (self):= MOVINGTOUNLOADPOS
		endif

//RUle that defines the MovingUnload

	rule r_MovingUnload =
		if currphasee (self)= MOVINGTOUNLOADPOS and unloadpositionreached = true then
			currphasee (self):= STOPPEDINUNLOADPOS
		endif

//Rule that defines the MovingLoad

	rule r_MovingLoad =
		if currphasee (self)= MOVINGTOLOADPOS and loadpositionreached = true then
			currphasee (self):= STOPPEDINLOADPOS2
		endif

//Rule that defines the WaitingUnload

	rule r_WaitingUnload=
		if currphasee (self)= STOPPEDINUNLOADPOS and tableloaded = false then
			currphasee (self):= MOVINGTOLOADPOS
		endif

//Rules for Agent "ROBOT"

//Rules that define the waiting

	rule r_Waiting1 =
		if currphaser (self)= WAITINGINUNLOADTABLEPOS and
			tablereadyforunloading (self)= true then
			currphaser (self) := UNLOADINGTABLE
		endif

	rule r_Waiting2 =
		if currphaser (self)=WAITINGINUNLOADPRESSPOS and
			pressreadyforunloading (self)=true then
			currphaser (self):= UNLOADINGPRESS
		endif

	rule r_Waiting3 =
		if currphaser (self)=WAITINGINLOADDEPBELTPOS and
			depositbeltreadyforloading (self)=true then
			currphaser (self):= LOADINGDEPBELT
		endif

	rule r_Waiting4 =
		if currphaser (self)=WAITINGINLOADPRESSPOS and pressreadyforloading (self)=true then
			currphaser (self):= LOADINGPRESS
		endif

//Rules that defines the action

	rule r_Action1 =
		if currphaser (self) = UNLOADINGTABLE and unloadtablecomp = true then
			seq
				currphaser (self):= MOVINGTOUNLOADPRESSPOS
				tableloaded := false
			endseq
		endif

	rule r_Action2 =
		if currphaser (self)= UNLOADINGPRESS and unloadpresscomp =true then
			seq
				currphaser (self):= MOVINGTOLOADDEPBELTPOS
				pressloaded := false
			endseq
		endif

	rule r_Action3 =
		if currphaser (self)=LOADINGDEPBELT and loaddepbeltcomp =true then
			seq
				currphaser(self):= MOVINGTOLOADPRESSPOS
				depositbeltreadyforloading(self) := false
			endseq
		endif

	rule r_Action4 =
		if currphaser (self)= LOADINGPRESS and loadpresscomp=true then
			seq
				currphaser(self):= MOVINGTOUNLOADTABLEPOS
				pressloaded := true
			endseq
		endif

//Rules that defines the Moving

	rule r_Moving1 =
		if currphaser (self)= MOVINGTOUNLOADPRESSPOS and unloadpressposreached = true then
			currphaser (self):= WAITINGINUNLOADPRESSPOS
		endif

	rule r_Moving2 =
		if currphaser (self)= MOVINGTOLOADDEPBELTPOS and loaddepbeltposreached = true then
			currphaser (self):= WAITINGINLOADDEPBELTPOS
		endif

	rule r_Moving3 =
		if currphaser (self)= MOVINGTOLOADPRESSPOS and loadpressposreached = true then
			currphaser (self):= WAITINGINLOADPRESSPOS
		endif

	rule r_Moving4 =
		if currphaser (self)= MOVINGTOUNLOADTABLEPOS and unloadtableposreached = true then
			currphaser (self):= WAITINGINUNLOADTABLEPOS
		endif

//Rules for Agent "PRESS"

//Rule that defines the Waiting_Unload

	rule r_Waiting_Unload =
		if currphasep (self) = OPENFORLOAD and pressloaded = false then
			currphasep (self):= MOVINGTOMIDDLEPOSITION
		endif

//Rule that defines the Moving to Middle

	rule r_Moving_To_Middle =
		if currphasep (self)= MOVINGTOMIDDLEPOSITION and middleposition= true then
			currphasep (self):= OPENFORLOAD
		endif

//Rule that defines the Waiting_Load

	rule r_Waiting_Load =
		if currphasep (self)= OPENFORLOAD and pressloaded= true then
			currphasep (self):= MOVINGTOTOPPOSITION
		endif

//Rule that defines the Moving_To_Upper

	rule r_Moving_To_Upper =
		if currphasep (self)= MOVINGTOTOPPOSITION and topposition= true then
			currphasep (self):= CLOSEDFORFORGIN
		endif

//Rule that defines the Closed

	rule r_Closed =
		if currphasep (self)= CLOSEDFORFORGIN and forgincompleted= true then
			currphasep (self):= MOVINGTOBOTTOMPOSITION
		endif

//Rule that definse the Moving_To_Lower

	rule r_Moving_To_Lower =
		if currphasep (self)= MOVINGTOBOTTOMPOSITION and bottomposition= true then
			currphasep (self):= OPENFORLOAD
		endif

//Rules for Agent "DEPOSIT"

//Rule that defines the Db_Normal

	rule r_Db_Normal =
		if currphased (self)= NORMAL and pieceindepositbeltlightbarrier =true then
			currphased (self):= CRITICAL
		endif

//Rule that defines the Db_Critical

	rule r_Db_Critical =
		if currphased (self)= CRITICAL and pieceindepositbeltlightbarrier =false then
		seq
			currphased (self):= STOP
			depositbeltreadyforloading (self) := true
			pieceatdepositbeltend := true
		endseq
		endif

//Rule that defines the Db_Stopped

	rule r_Db_Stopped =
		if currphased (self)= STOP and  pieceatdepositbeltend = false then
			currphased (self):= NORMAL
		endif

//Rules for Agent "CRANE"

//Rule that defines th waiting

	rule r_Waiting_Db =
		if currphasec (self)= WAITINGTOUNLOADDEPOSITBELT and
			pieceatdepositbeltend = true then
			currphasec (self):= UNLOADINGDEPOSITBELT
		endif

//RUle that defines the Unloading

	rule r_Unloading_Db =
		if currphasec (self)= UNLOADINGDEPOSITBELT and
			unloadingdepositbeltcompleted = true then
			seq
				currphasec (self):= MOVINGTOLOADFEEDBELTPOS
				pieceatdepositbeltend:=false
			endseq
			endif

//Rule that defines the Moving

	rule r_Moving_Fb =
		if currphasec (self)=  MOVINGTOLOADFEEDBELTPOS and loadfeedbeltposreached = true then
			currphasec (self):= WAITINGLOADFEEDBELT
		endif

//Rule that defines the waiting (Fb)

	rule r_Waiting_Fb =
		if currphasec (self)=WAITINGLOADFEEDBELT and feedbeltfree = true then
			currphasec (self):= LOADINGFEEDBELT
		endif

//Rule that defines the Loading (Fb)

	rule r_Loading_Fb =
		if currphasec (self)= LOADINGFEEDBELT and loadingdepositbeltcompleted = true then
			seq
				currphasec (self):= MOVINGTOUNLOADDEPOSITBELTPOS
				feedbeltfree:=false
			endseq
			endif

//Rule that defines the Moving (Db)

	rule r_Moving_Db =
		if currphasec (self)=   MOVINGTOUNLOADDEPOSITBELTPOS and
			unloaddepositbeltposreached = true then
			currphasec (self):= WAITINGTOUNLOADDEPOSITBELT
		endif



//Specific agent programs

rule r_TransportAgent = seq
				r_FbNormal []
				r_Change []
				r_FbStopped []
				r_FbCritical []
                        endseq

rule r_ERTAgent = seq
				r_WaitingLoad []
				r_MovingUnload []
				r_MovingLoad []
				r_WaitingUnload []
			endseq

rule r_DEPOSITAgent =
            seq
				r_Db_Normal []
				r_Db_Critical []
				r_Db_Stopped []
			endseq

rule r_CRANEAgent =
            seq
				r_Waiting_Db []
				r_Unloading_Db []
				r_Moving_Fb []
				r_Waiting_Fb []
				r_Loading_Fb []
				r_Moving_Db []
			endseq

rule r_PRESSAgent =
            seq
				r_Waiting_Unload []
				r_Moving_To_Middle []
				r_Waiting_Load []
 				r_Moving_To_Upper []
				r_Closed []
				r_Moving_To_Lower []
			endseq

rule r_ROBOTAgent=
            seq
				r_Waiting1 []
				r_Waiting2 []
				r_Waiting3 []
				r_Waiting4 []
				r_Action1  []
				r_Action2  []
				r_Action3  []
				r_Action4  []
				r_Moving1  []
				r_Moving2  []
				r_Moving3  []
				r_Moving4  []
			endseq


             // run all the agents
	invariant inv_abc  over tokens: hallo and @original

	main rule r_prod_cell = par
		program(transport)
		program(ert)
		program(robot)
		program(press)
		program(deposit)
		program(crane)
	endpar

//define the initial states

default init initial_state:

//function for Agent "TRANSPORT"
	function currphaseTra ($a in TransportAgent) =  NORMALRUN
	function piecefeedbeltlightbarrier = false
	function feedbeltfree = true

//function for Agent "ROBOT", "DEPOSIT"
	function depositbeltreadyforloading($a in Agent) = @original($b)

//function for Agent "DEPOSIT", "CRANE"
	function pieceatdepositbeltend = false

//AGENTS initialization

agent TransportAgent :          r_TransportAgent[]
agent ERTAgent  : r_ERTAgent[]

agent DEPOSITAgent  : r_DEPOSITAgent[]

agent CRANEAgent  : r_CRANEAgent[]

agent PRESSAgent  :  r_PRESSAgent[]

agent ROBOTAgent : r_ROBOTAgent[]




