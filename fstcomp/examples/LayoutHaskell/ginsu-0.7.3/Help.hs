module Help(
    bugMsg,
    getHelpTable,
    usageHeader,
    usageTrailer
    ) where

import GenUtil(buildTableRL, indentLines)
import KeyName

{-# NOINLINE usageHeader #-}
{-# NOINLINE usageTrailer #-}
{-# NOINLINE bugMsg #-}
{-# NOINLINE getHelpTable #-}

usageHeader :: String
usageTrailer :: String
bugMsg :: String

usageHeader = "Usage: ginsu [OPTION...] categories..."

usageTrailer = unlines [
    "For more information see the homepage at",
    "   <http://repetae.net/john/computer/ginsu/>",
    "To report a bug go to:",
    "   <http://bugs.ofb.net/cgi-bin/bugzilla/enter_bug.cgi?product=Ginsu>",
    "When reporting a bug, include the last few lines of ~/.gale/ginsu.errorlog if it seems appropriate."
    ]

bugMsg = unlines [
    "There has been an internal error",
    "verify the output of ginsu --checkconfig and",
    "please inform <john@ugcs.caltech.edu> or file a bug report at",
    "<http://bugs.ofb.net/cgi-bin/bugzilla/enter_bug.cgi?product=Ginsu>",
    "Include the following text in any error report:"
    ]




getHelpTable = do
    ht <- getKeyHelpTable (0,0)
    return ("Keybindings:\n" ++ indentLines 2 ht ++ filters ++ "\nFor more info see the manual at\n  http://repetae.net/john/computer/ginsu/ginsu-manual.html\n")


filters = "\nFilter Reference:\n" ++ indentLines 2 (unlines (buildTableRL [
    ("",""),
    ("Primitive Filters:",""),
    (" ~a:<gale-id>","author of puff"),
    (" ~c:<category>","category puff was sent too"),
    (" ~k:<regex>","regex match against keyword"),
    (" ~s:<regex>","regex match against senders real name"),
    (" ~b:<regex>","regex match against message body"),
    (" /<regex>","search for <regex> in visible fragments"),
    ("",""),
    ("Combining Filters:",""),
    (" A ; B", "Filter A or filter B"),
    (" A B", "Filter A and filter B"),
    (" !A", "Not filter A"),
    (" (A)", "Grouping, same as filter A"),
    (" /'foo bar'", "Single quotes are used to quote strings")
    ])) ++ "\n" ++ filterExamples


filterExamples = "Filter Examples:\n"  ++ indentLines 4 (unlines es) where
    es = [
	"(~a:john@ugcs.caltech.edu)\n  all puffs by john@ugcs",
	"(~c:pub.tv.buffy ~a:jtr@ofb.net)\n  puffs from jtr and to pub.tv.buffy",
	"(/ginsu ; ~c:pub.gale.ginsu)\n  puffs containing the word ginsu or directed to pub.comp.ginsu",
	"(!~k:spoil)\n  no spoilers",
	"(~c:pub.tv.buffy !~c:pub.meow)\n  puffs to buffy which are not about cats"
	]

{-

paragraph maxn xs = drop 1 (f maxn (words xs)) where
    f n (x:xs) | lx <- length x + 1, lx < n = (' ':x) ++ f (n - lx) xs
    f n (x:xs) = '\n': (x ++ f (maxn - length x) xs)
    f n [] = "\n"

 keys = buildTableRL [
    ("Keys:", ""),
    ("<F1>", "Help Screen"),
    ("<F2>", "Main Screen"),
    ("<F3>", "Presence Status Screen"),
    ("",""),
    ("j","next puff"),
    ("k","previous puff"),
    ("<Home>","first puff"),
    ("<End>","last puff"),
    ("<Enter> <C-E>","forward one line"),
    ("<BackSpace> <C-Y>","back one line"),
    ("<C-F> <PgDown>","forward one page"),
    ("<C-B> <PgUp>","back one page"),
    ("<C-D>","forward one half-page"),
    ("<C-U>","back one half-page"),
    ("",""),
    ("d","Show puff details"),
    ("v","Visit links in puff"),
    ("",""),
    ("c ~","add new filter"),
    ("u","pop one filter"),
    ("U","pop all filters"),
    ("!","invert filter at top of stack"),
    ("x","swap top two filters on stack"),
    ("a","filter to current author"),
    ("t","filter to current thread"),
    ("T","filter to strict thread"),
    ("m[1-9]","set mark - save current filter stack at given key"),
    ("[1-9]","recall filter stack saved here with 'm'"),
    ("C[1-9]","recall filter stack and add it to current filter stack"),
    ("",""),
    ("f","follow-up to category"),
    ("p","compose new puff"),
    ("r","reply to author"),
    ("g","group reply, sends to recipients and author of puff"),
    ("R","resend selected puff"),
    ("N","modify public presence string"),
    ("",""),
    ("<C-R>","reconnect to all servers"),
    ("E","edit and reload configuration file"),
    ("q Q","quit program"),
    ("<C-L>","redraw screen")
    ]

-}
