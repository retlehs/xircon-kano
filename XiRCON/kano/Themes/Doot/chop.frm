##
# format info
# -all 3 should be set

frmset name "chop"
frmset author "pawt"
frmset desc "an example format file, to learn from.."

##
# ansi
# -not required
# -yeah, this one is ugly, but hey, it's an example! =)

frmset ansi {
16²²²²²²²²²²²²²²²²²²²²²²²²²15²14²
16²²
16²²     14²²  ²² 15²²²²²² 16²²²²²²
16²²     14²²²²²² 15²²  ²² 16²²  ²²
16²²     14²²  ²² 15²²  ²² 16²²²²²²
16²²     14²²  ²² 15²²²²²² 16²²
16²²
16²²²²²²²²²²²²²²²²²²²²²²²²²15²14²
}

##
# color schemes
# -if you don't define a 'default' scheme, you must use
#  setcolor to tell doot which is the default scheme
# -you can define up to 9 colors for each name
# -the first set is for your formats, the second set is for doot

setcolor darkred
addcolfrm darkred "14 16 5 15" "15 14 5"
addcolfrm lightred "14 16 4 15" "15 14 4"
addcolfrm darkgreen "14 16 3 15" "15 14 3"
addcolfrm lightgreen "14 16 9 15" "15 14 9"
addcolfrm darkblue "14 16 2 15" "15 14 2"
addcolfrm lightblue "14 16 12 15" "15 14 12"
addcolfrm darkyellow "14 16 7 15" "15 14 7"
addcolfrm lightyellow "14 16 8 15" "15 14 8"
addcolfrm darkpurple "14 16 6 15" "15 14 6"
addcolfrm lightpurple "14 16 13 15" "15 14 13"
addcolfrm mono "14 16 16 15" "15 14 16"

##
# pres/bullets
# -one is required, any others are optional
# -you can use %# for colors in your bullets

addpre %c1o%c4o%c2o%c1
addpre %c1Ä%c4Ä%c2>%c1
addpre %c2<%c4Ä%c1Ä%c1
addpre %c3ù%c1

##
# formats
# -every single one should be defined, but the default xircon will be shown if it isn't
# -read formats.txt for info on each format, and additional info on making format files

# text formats
addform owntext        "%c3\[%c2%lnick%c3\]%c1 %text"
addform chantext       "%c3\[%c4%lnick%c3\]%c1 %text"
addform privtext       "%c3\[%c4%lnick%c3\]%c1 %text"
addform chattext       "%c3\[%c4%lnick%c3\]%c1 %text"
addform hightext       "%c2\[%c1%lnick%c2\]%c1 %text"
addform action         "%pre4 %c2%nick %c1%text"

# time and address
addform time           "%c1%h%c3:%c1%m%a"
addform addy           "%c2!%c1 %c2@%c1 %c3.%c1"

# channel-related formats
addform join           "%pre1 %c2%nick%c3(%c1%user%c2@%c1%thost%c3) %c4has joined %c2#%c1%nchan %c2@%ttime"
addform part           "%pre1 %c2%nick%c3(%c1%user%c2@%c1%thost%c3) %c4has left %c2#%c1%nchan %c2@%ttime"
addform quit           "%pre1 %c2%nick%c3(%c1%user%c2@%c1%thost%c3) %c4has quit %c2#%c1%nchan %c3(%c1%reason%c3) %c2@%ttime"
addform kick           "%pre1 %c2%nick %c4kicked %c2%knick %c4from %c2#%c1%nchan %c3(%c1%reason%c3) %c2@%ttime"
addform nick           "%pre1 %c2%oldnick%c3(%c1%user%c2@%c1%thost%c3) %c4is now known as %c2%newnick %c2@%ttime"
addform mode           "%pre1 %c2%nick%c3(%c1%user%c2@%c1%thost%c3) %c4sets mode %c3(%c1%mode %parms%c3) %c4on %c2#%c1%nchan %c2@%ttime"
addform chanmode       "%pre1 %c2%nick%c3(%c1%user%c2@%c1%thost%c3) %c4sets mode %c3(%c1%mode%c3) %c4on %c2#%c1%nchan %c2@%ttime"
addform topic          "%pre1 %c2%nick %c4changed the topic to %c3(%c1%topic%c3) %c2@%ttime"

# private formats
addform usermode       "%pre1 %c4usermode now %c3(%c4%mode%c3) %c2@%ttime"
addform inmsg          "%pre3 %c4msg%c3.%c2%nick%c3(%c1%user%c2@%c1%thost%c3)%c1 %text"
addform outmsg         "%pre2 %c4msg%c3.%c2%nick%c1 %text"
addform ctcp           "%pre3 %c4ctcp%c3.%c2%nick%c3(%c1%user%c2@%c1%thost%c3)%c1 %ctcp %parms"
addform outctcp        "%pre2 %c4ctcp%c3.%c2%nick%c1 %ctcp %parms"
addform chanctcp       "%pre3 %c4ctcp%c3.%c2%nick%c3(%c1%user%c2@%c1%thost%c3) %c4to %c2#%c1%nchan%c3: %c1%ctcp %parms"
addform ctcprep        "%pre3 %c4%lctcp reply%c3.%c2%nick%c3(%c1%user%c2@%c1%thost%c3) %c1%reply"
addform invite         "%pre3 %c4invite%c3.%c2%nick%c3(%c1%user%c2@%c1%thost%c3) %c4to %c2#%c1%nchan %c2@%ttime"
addform notice         "%c2-%c3(%c1%nick%c2!%c1%user%c2@%c1%host%c3)%c2-%c1 %text"
addform channotice     "%c3(%c1%nick%c2!%c1%user%c2@%c1%host%c3.%c2#%c1%nchan%c3)%c1 %text"
addform outnotice      "%pre2 %c4notice%c3.%c2%nick%c1 %text"

# misc. stuff
addform timenow        "%pre1 %c4the time is %ttime"
addform awaypage       "is away - %reason (+p)"
addform awaynopage     "is away - %reason"
addform back           "is back - %reason (%gone)"
addform notify         "%pre1 %c2%nick%c3(%c1%user%c2@%c1%thost%c3) %c4is on irc %c2@%ttime"
addform denotify       "%pre1 %c2%nick%c3(%c1%user%c2@%c1%thost%c3) %c4is not on irc %c2@%ttime"
addform onotice        "(%chan.%ops) %text"
addform kill           "%pre1 %c2%oper%c3(%c1%kills%c3) %c4killed %c2%nick %c3(%c1%reason%c3) %c2@%ttime"

# dcc formats
addform sendcreate     "%pre1 %c4dcc%c3.%c2send%c3: %c2%nfile%c3(%c1%size%c3) %c4to %c2%nick"
addform sendbegin      "%pre1 %c4dcc%c3.%c2send%c3: %c4sending %c2%nfile%c3(%c1%size%c3) %c4to %c2%nick"
addform senddone       "%pre1 %c4dcc%c3.%c2send%c3: %c4done sending %c2%nfile%c3(%c1%size%c3) %c4to %c2%nick %c3(%c1%cpsk/s%c3.%c1%time%c3)"
addform senderror      "%pre1 %c4dcc%c3.%c2send%c3: %c4error sending %c2%nfile %c4to %c2%nick"
addform getcreate      "%pre1 %c4dcc%c3.%c2get%c3: %c2%nfile%c3(%c1%size%c3) %c4from %c2%nick"
addform getbegin       "%pre1 %c4dcc%c3.%c2get%c3: %c4receiving %c2%nfile%c3(%c1%size%c3) %c4from %c2%nick"
addform getdone        "%pre1 %c4dcc%c3.%c2get%c3: %c4done receiving %c2%nfile%c3(%c1%size%c3) %c4from %c2%nick %c3(%c1%cpsk/s%c3.%c1%time%c3)"
addform geterror       "%pre1 %c4dcc%c3.%c2get%c3: %c4error receiving %c2%nfile %c4from %c2%nick"

# dns formats
addform dnsattempt     "%pre1 %c4dns%c3.%c2attempt%c3: %c4looking up %c3(%c1%address%c3)"
addform dnsnickattempt "%pre1 %c4dns%c3.%c2attempt%c3: %c4looking up %c2%nick%c3(%c1%address%c3)"
addform dnsresolve     "%pre1 %c4dns%c3.%c2resolve%c3: %c4resolved %c3(%c1%address%c3) %c4to %c3(%c1%resolve%c3)"
addform dnsnoresolve   "%pre1 %c4dns%c3.%c2resolve%c3: %c4unable to resolve %c3(%c1%address%c3)"

# whois formats
addform whoistop       "%c4ÚÄÄÄ%c1- %c2whois %c1-%c4ÄÄÄ%c1-"
addform whoisinfo      "%c4³ %c2info        %c3: %c2%nick%c3(%c1%user%c2@%c1%thost%c3) %c3(%c1%type%c3)"
addform whoisname      "%c4³ %c2name        %c3: %c4%name"
addform whoischan      "%c4³ %c2channels    %c3: %c4%chans"
addform whoisserv      "%c4³ %c2server      %c3: %c2%serv %c3(%c1%info%c3)"
addform whoisidle      "%c4³ %c2idle        %c3: %c4%idle"
addform whoison        "%c4³ %c2on          %c3: %c4%on"
addform whoisircop     "%c4³ %c2ircop       %c3: %c2%nick %c4has the power of the peepee"
addform whoisaway      "%c4³ %c2away        %c3: %c4%reason"
addform whoisbottom    "%c4ÀÄÄÄ%c1- %c2whois %c1-%c4ÄÄÄ%c1-"

# whowas formats
addform whowastop       "%c4ÚÄÄÄ%c1- %c2whowas %c1-%c4ÄÄÄ%c1-"
addform whowasinfo      "%c4³ %c2info         %c3: %c2%nick%c3(%c1%user%c2@%c1%thost%c3) %c3(%c1%type%c3)"
addform whowasname      "%c4³ %c2name         %c3: %c4%name"
addform whowasserv      "%c4³ %c2server       %c3: %c2%serv %c3(%c1%time%c3)"
addform whowasbottom    "%c4ÀÄÄÄ%c1- %c2whowas %c1-%c4ÄÄÄ%c1-"

# titlebar formats
addform titlequery      "query with %nick( %user@%host ) @%time"
addform titlechat       "chat with %nick( %user@%host ) @%time"
addform titlechannel    "%chan( %total total, %op op, %voice voice, %non non ) @%time"
addform titlefinger     "finger( %user@%host ) @%time"
addform titlestatus     "%nick( +%mode ) on %server( %port ) @%time"

# box formats
addform boxtop          "%c4ÚÄÄÄÄÄÄ%c1- -%c3Ä%c1- -%c4ÄÄÄÄ%c1- -%c4ÄÄ%c1- %c2%title %c1-%c4ÄÄÄÄÄÄÄÄ%c1- -%c3Ä%c1- -%c4ÄÄÄ%c1-"
addform boxtop2         "%c4ÚÄÄÄÄÄÄ%c1- -%c3Ä%c1- -%c4ÄÄÄÄ%c1- -%c4ÄÄ%c1- %c2%title%c3(%c1%title2%c3) %c1-%c4ÄÄÄÄÄÄÄÄ%c1- -%c3Ä%c1- -%c4ÄÄÄ%c1-"
addform boxside         "%c4³%c2"
addform boxbottom       "%c4ÀÄÄÄ%c1- -%c3Ä%c1- -%c4ÄÄÄÄÄÄÄ%c1- %c4%text"

# bracket formats
addform bracket         "%c3(%c1%text%c3)"
addform bracket2        "%c3\[%c1%text%c3(%c2%text2%c3)\]"

# /users formats
addform userchars       "%c1@%c2 %c1+%c2"
addform usertop         "%pre1 %c1users%c3\[%c1%chan%c3(%c2+o%c3/%c1%ops %c2+v%c3/%c1%voice %c2+n%c3/%c1%non%c3)\]"
addform usernicks       "%pre1 %c3(%c2%nick1%c3) %c3(%c2%nick2%c3) %c3(%c2%nick3%c3) %c3(%c2%nick4%c3) %c3(%c2%nick5%c3)"
addform userbottom      ""
