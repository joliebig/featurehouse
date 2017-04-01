'''

composes fstcomp examples

You may have to run `ant jar` beforehand, to create the featurehouse jar file.
Make sure, you have the featurehouse_fstcomp_examples checked out at fstcomp/examples.

Run the script from the scripts directory:

$ cd fstcomp/scripts
$ python compose_examples.py ../../jar/featurehouse_xxx.jar ../examples ./output


This will compose some of the examples in a directory named output that will be created in the scripts folder.

'''


from batch_fstcomp import *

EXAMPLES = """Java/GameOfLife/GameOfLifeComp.features
Java/Notepad/NotepadChadWellington/NotepadComp.features
Java/Notepad/NotepadBenDelaware/NotepadComp.features
Java/Notepad/NotepadLuisGuimbarda/NotepadComp.features
Java/Notepad/NotepadGanovSvetoslav/NotepadComp.features
Java/Notepad/NotepadLukeRobison/NotepadComp.features
Java/Notepad/NotepadAdrianQuark/NotepadComp.features
Java/Notepad/NotepadIanWehrman/NotepadComp.features
Java/ChatSystem/ChatSystemThomasThuem/ChatSystemComp.features
Java/ChatSystem/ChatSystemAlexanderBurke/ChatSystemComp.features
Java/ChatSystem/ChatSystemHagenSchink/ChatSystemComp.features
Java/ChatSystem/ChatSystemAlexanderDreiling/ChatSystemComp.features
Java/ChatSystem/ChatSystemChristopherWeiss/ChatSystemComp.features
Java/ChatSystem/ChatSystemSonLuong/ChatSystemComp.features
Java/ChatSystem/ChatSystemThomasRehn/ChatSystemComp.features
Java/ChatSystem/ChatSystemChristianBecker/ChatSystemComp.features
Java/ChatSystem/ChatPL1/ChatPLComp.features
Java/Graph/GraphComp.features
Java/TankWar/TankWarComp.features
Java/Prevayler/PrevaylerComp.features
Java/AJStats/AJStatsComp.features
Java/BerkeleyDB/BerkeleyDBComp.features
Java/MobileMedia8/MobileMedia8Comp.features
Java/Violet/VioletComp-fuji.features
Java/Violet/VioletComp.features
Java/Bali/Bali2JavaCCComp.features
Java/Bali/Bali2LayerComp.features
Java/Bali/Bali2JakComp.features
Java/Bali/BaliComposerComp.features
Java/GUIDSL/GUIDSLComp.features
Java/GPL/GPLComp.features
Java/Raroscope/RaroscopeComp.features
Java/Test/Store.features
Java/EPL/EPLComp.features
Java/PKJab/PKJabComp.features
Java/ZipMe/ZipMeComp.features
Java/AHEAD/JampackComp.features
Java/AHEAD/MixinComp.features
Java/AHEAD/MMatrixComp.features
Java/AHEAD/Jak2JavaComp.features
Java/AHEAD/JRENameComp.features
Java/AHEAD/UnMixinComp.features
Java/AHEAD/BCJak2JavaComp.features
Java/Sudoku/SudokuComp.features
Exprlang/ExprlangComp.features
UML/AudioControlSystem/AudioControlSystemComp.features
UML/ConferenceManagementSystem/ConferenceManagementSystemComp.features
UML/ConferenceManagementSystem/CMSComp.features
UML/GasBoilerSystem/GasBoilerSystemComp.features
UML/PhoneSystem/PhoneSystemComp.features
UML/PhoneSystem/PhoneComp.features
JavaCC/FFJ/FFJComp.features
FJ/Simple/SimpleComp.features
CSharp/Graph/GraphComp.features
CSharp/GPL/GPLComp.features
Haskell/FGL/FGLComp.features
Haskell/Arith/ArithComp.features
MoBL/MoBLComp_Base_HTML_OrderBy.features
MoBL/MoBLComp_Base_HTML_Async_Service_OrderBy.features
MoBL/MoBLComp_Base_HTML.features
MoBL/MoBLComp_Base_HTML_Async_Service.features
MoBL/MoBLComp_Base_HTML_Async.features
MoBL/MoBLComp_Base_HTML_Service.features
MoBL/MoBLComp_Base_HTML_Async_OrderBy.features
MoBL/MoBLComp_Base_HTML_Service_OrderBy.features
StrategoTest/Stratego/StrategoComp.features
Alloy/POSIXFileSystem/POSIXFileSystemComp.features
Alloy/Graph/GraphComp.features
Alloy/Phone/PhoneComp.features
Alloy/CAN/CANComp.features
C/EmailSystem/src/EmailSystemComp.features
C/GraphLib/GraphLibComp.features""".split()

COMPOSITION_PLAN = (
    products(EXAMPLES)
)

if __name__ == '__main__':
    batch_compose_from_cmdline(
        COMPOSITION_PLAN
    )
