# doot b2 format file -- 8/10/98
# Use Ms LineDraw or I will come to your house and hurt you.
# Also, if you see hellish, tell him his style rocks. I got alot of this from him.

# Set this stuff so you know who made the damned thing.

frmset name   "Bleak"
frmset author "Lava"
frmset desc   "Cutting Edge Style.."
frmset file   "doot.tcl"

# Add some fruity color schemes (more to come)

setcolor blues
addcolfrm blues "12 16 15 14 8 2" "16 12 15"

# Bullets for my gun.

addpre "%c1ù"
addpre "%c1!"
addpre "%c3!%c2!%c1!"

# Get down to the heart of the file.

addform owntext        "%c4>%c3 %text"
addform chantext       "%c1\[%c2%nick%c1\]%c3 %text"
addform privtext       "%c1\[%c2%nick%c1\]%c3 %text"
addform chattext       "%c1\[%c2%nick%c1\]%c3 %text"
addform hightext       "%c1\[%c5%nick%c1\]%c3 %text"
addform action         "%pre2 %c2%nick%c3 %text"

addform time           "%c2%h%c1:%c2%m%a"
addform addy           "%c3!%c2 %c3@%c2 %c1.%c2"

addform join           "%pre1 %c1\[%c2join%c1\] \[ %c2%nick %c4<%c3%user%c4@%c3%host%c4> %c1\]"
addform part           "%pre1 %c1\[%c2part%c1\] \[ %c2%nick %c4<%c3%user%c4@%c3%host%c4> %c1\]"
addform quit           "%pre1 %c1\[%c2quit%c1\] %c2%nick %c4(%c3%reason%c4)"
addform kick           "%pre1 %c2\[%c2Kick%c1\] %c2%nick%c3 kicked %c2%knick %c4(%c3%reason%c4)"
addform nick           "%pre1 %c2%oldnick%c3 is now knows as: %c2%newnick"

addform mode           "%pre1 %c1\[%c2mode%c1\] %c2%nick%c1\[%c3%user%c4@%c3%host%c1\]%c3 %mode %parms"
addform chanmode       "%pre1 %c1\[%c2mode%c1\] %c2%nick%c1\[%c3%user%c4@%c3%host%c1\]%c3 %mode"
addform usermode       "%pre1 %c2Usermode now set to%c3:%c2 %mode"

addform inmsg          "%c1\[%c2%nick%c4(%c3%user%c4@%c3%host%c4)%c1\]%c3 %text"
addform outmsg         "%c1\[%c2msg%c4(%c2%nick%c4)%c1\]%c3 %text"
addform topic          "%pre1 %c1\[%c2topic%c1\] %c2%nick%c3 changed topic to: %topic"
addform timenow        "%pre1 %c3At the tone, the time will be %c2%ttime%c3 ..beep"

addform outctcp        "%pre1 %c1\[%c2ctcp%c4(%c2%nick%c4)%c2\]%c3 %ctcp %parms"
addform ctcp           "%pre1 %c2%nick%c3 requested a %c2%ctcp %parms%c3 from you."
addform chanctcp       "%pre1 %c2%nick%c3 requested a %c2%ctcp %parms%c3 from %chan."
addform ctcprep        "%pre1 %c2%ctcp%c3 reply from %c2%nick%c3: %reply"

addform invite         "%pre1 %c2%nick%c3 invites you to join: %c2%chan"

addform awaypage       "will bbl. \[pAGING:yEP\]"
addform awaynopage     "will bbl."
addform back           "is back."

addform notify         "%pre3 %c2%nick %c1\[%c3%user%c4@%c3%host%c1\]%c4:%c3 on irc."
addform denotify       "%pre3 %c2%nick %c1\[%c3%user%c4@%c3%host%c1\]%c4:%c3 no longer on irc."
addform onotice        "\[@%chan\]: %text"

addform sendcreate     "%pre1 %c3Trying to send %c2%nick %c1\[%c2%nfile%c1\]"
addform getcreat       "%pre1 %c2%nick%c3 is trying to send you %c1\[%c2%nfile%c1\]"
addform sendbegin      "%pre1 %c3Sending %c1\[%c2%nfile%c1\]%c3 to %c2%nick"
addform getbegin       "%pre1 %c3Recieving %c1\[%c2%nfile%c1\]%c3 from %c2%nick"
addform senddone       "%pre1 %c3Send complete to %c2%nick%c3:%c2 %nfile %c4<%c3%cpsk/s%c4>"
addform getdone        "%pre1 %c3Get complete from %c2%nick%c3:%c2 %nfile %c4<%c3%cpsk/s%c4>"
addform senderror      "%pre1 %c3Failed to send to %c2%nick%c3:%c2 %nfile"
addform geterror       "%pre1 %c3Failed to recieve from %c2%nick%c3:%c2 %nfile"

addform dnsattempt     "%pre1 %c1\[%c2dns%c1\]%c4:%c3 Attempting to resolve %c2%address"
addform dnsnickattempt "%pre1 %c1\[%c2dns%c1\]%c4:%c3 Attempting to resolve %c2%nick%c4\[%c3%address%c4\]"
addform dnsresolve     "%pre1 %c1\[%c2dns%c1\]%c4:%c3 Resolved %c2%address %c3->%c2 %resolve"
addform dnsnoresolve   "%pre1 %c1\[%c2dns%c1\]%c4:%c3 Could not resolve %c2%address"

addform notice         "%c1-%c2%nick%c1-%c3 %text"
addform channotice     "%c1-%c2%nick%c6:%c2%chan%c1-%c3 %text"
addform outnotice      "%c1\[%c2notice%c4(%c2%nick%c4)%c1\]%c3 %text"

addform kill           "%pre3 %c2%oper %c3killed %c2%nick %c4(%c3%reason%c4)"

addform titlequery     "\[Query:%nick\] \[Idle:%idle\] \[Time:%time\]"
addform titlechat      "\[Chat:%nick\] \[Idle:%idle\] \[Time:%time\]"
addform titlechannel   "\[Channel:%chan\] \[Idle:%idle\] \[Time:%time\]"
addform titlefinger    "\[Finger:%user@%host\] \[Idle:%idle\] \[Time:%time\]"
addform titlestatus    "\[Status\] %nick(%user@%host) \[Mode:+%mode\] \[%server:%port\] \[Idle:%idle\]"

addform boxtop         "%c4ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ%c1\[ %c2%title %c1\]%c4ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ%c3Ä%c2Ä"
addform boxtop2        "%c4ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ%c1\[ %c2%title %c4<%c3%title2%c4> %c1\]%c4ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ%c3Ä%c2Ä"
addform boxside        "%c4³"
addform boxbottom      "%c4ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ%c3Ä%c2Ä %c3%text"

addform whoistop       "%c4ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ%c1\[ %c2%nick is.. %c1\]%c4ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ%c3Ä%c2Ä"
addform whoisinfo      "%c4³ %c1\[%c2address   %c1\] \[%c3%user%c4@%c3%host %c2> %c3%type%c1\]"
addform whoisname      "%c4³ %c1\[%c2ircname   %c1\] \[%c3%name%c1\]"
addform whoischan      "%c4³ %c1\[%c2channels  %c1\] \[%c3%chans%c1\]"
addform whoisserv      "%c4³ %c1\[%c2server    %c1\] \[%c3%serv%c1\]"
addform whoisidle      "%c4³ %c1\[%c2idle      %c1\] \[%c3%idle%c1\]"
addform whoison        ""
addform whoisircop     "%c4³ %c1\[%c2ircop     %c1\] \[%c3The IRC Gods have blessed %c2%nick%c1\]"
addform whoisaway      "%c4³ %c1\[%c2away      %c1\] \[%c3%reason%c1\]"
addform whoisbottom    "%c4ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ%c3Ä%c2Ä"

addform whowastop      "%c4ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ%c1\[ %c2%nick was.. %c1\]%c4ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ%c3Ä%c2Ä"
addform whowasinfo     "%c4³ %c1\[%c2address   %c1\] \[%c3%user%c4@%c3%host %c2> %c3%type%c1\]"
addform whowasname     "%c4³ %c1\[%c2ircname   %c1\] \[%c3%name%c1\]"
addform whowasserv     "%c4³ %c1\[%c2server    %c1\] \[%c3%serv%c1\]"
addform whowasbottom   "%c4ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ%c3Ä%c2Ä"

addform bracket        "%c1\[%c2%text%c1\]"
addform bracket2       "%c1\[%c2%text%c4(%c2%text2%c4)%c1\]"