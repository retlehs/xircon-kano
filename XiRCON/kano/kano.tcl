# .[kano.tcl version fifteen ]. .[now semi-modular for your convenience].
# .[keith@cs.oswego.edu      ]. .[www.kano.net                         ].
# .[kano on EFnet irc        ]. .[release date: ???? 1999              ].
# .[thanks to: conio for the amazing fix-mistakes
#   #xircon, vapid for the GetOps revision, xircon.com, and anyone who
#   sent me ideas, implemented or not                                  ].
# .[subarctic kicks ass, as does DaveG (DaveG == God)                  ].
# .[and thanks to cyclopse, my whore..and statik, my old syntax user =/].
# .[<Twiggy-2> put me on the greets cause I'm real real lame           ].
# .[thanks to m0g for the defaut /kb msg; that was a big part of kanoX ].
# .[and to morganti who redesigned my popups for k14 =D                ].
# .[and to the author of the tcl http package for FormatQuery          ].
# .[....and to Downtown for discovering a problem deep down in xircon's
#   storage of the userlist.                                           ].
# .[.......and thanks to reddd for helping me retest floodprot         ].
#
#  [DO AS I SAY]
# Type /help.
# Also, use a nice ANSI font like the one that came with the script. And for
#  God's sake, use a theme. Default blows. Haha.
#
#  [END OF INTRO]
# Try the /help command for everything else you need to know

if {![info exists kloaded]} {
    catch {
	namespace eval IRC {namespace export *}
	namespace import ::IRC::*
	foreach i {on alias hotkey menu} {
	    rename $i kano_$i
	    proc $i args "eval kano_ev [list $i] \$args"
	}
	proc kano_ev {type args} {
	    if {$type == "menu" && [llength $args] == 2} {eval kano_menu \
		    $args} else {eval [list kano_$type] \
		    [lrange $args 0 end-1] \
		    [list [list namespace eval :: [lindex $args end]]]}
	}
    }
    
    
    #thanks to tcl's http package
    
    # http::formatQuery --
    #
    #	See documentaion for details.
    #	Call http::formatQuery with an even number of arguments, where 
    #	the first is a name, the second is a value, the third is another 
    #	name, and so on.
    #
    # Arguments:
    #	args	A list of name-value pairs.
    # Results:
    #        TODO
    
    proc formatQuery {args} {
	set result ""
	set sep ""
	foreach i $args {
	    append result $sep [cgifix $i]
	    if {$sep != "="} {
		set sep =
	    } else {
		set sep &
	    }
	}
	return $result
    }

    proc cgifix phrase {
	set final_phrase ""
	for {set i 0} {$i < [string length $phrase]} {incr i} {
	    set x [string index $phrase $i]
	    scan $x %c asc
	    append final_phrase [format %%%.2X $asc]
	}
	return $final_phrase
    }

    set addglob [list]
    set kf [list]
    set mods [list]
    proc report_bug bug {
	global addglob kf mods
	if {[catch kano k]} {set k ***}
	if {[catch {socket -async www.kano.net 80} sock]} {
	    echo "$k error reporting bug: $sock"
	} else {
	    if {[catch {get_cookie mykanoserial 0} c]} {set c 0}
	    if {[catch {get_cookie theme} b]} {set b {}}
	    if {[catch intvers v]} {set v {}}
	    set bugreport($sock) "GET /bug.cgi?[formatQuery nick [my_nick] \
		    user [my_user] host [my_host] bug $bug addons $addglob \
		    theme $b first $kf mods $mods server [server] serial $c \
		    version $v tcl [info patchlevel]]"
	    fconfigure $sock -blocking 0 -buffering line
	    echo "$k 1 connecting to kano.net via http"
	    fileevent $sock writable "fileevent $sock writable {};set \
		    bugrep($sock) 1"
	    vwait bugrep($sock)
	    if {[catch {puts $sock $bugreport($sock)} er]} {echo \
		    "$k error reporting bug: $er";close $sock;return}
	    echo "$k 2 sending bug report"
	    fileevent $sock readable "set bugrep($sock) 1"
	    vwait bugrep($sock)
	    if {[catch {gets $sock line} er]} {echo \
		    "$k error reporting bug: $er\n$k this occurred while\
		    waiting for a confirmation from the server, meaning\
		    your bug may or may not have been reported";close \
		    $sock;return}
	    echo "$k 3 closing connection\n$k confirmation: $line"
	    close $sock
	}
    }

    set kloaded [clock clicks]
    if {[catch {source [info script]} error] == 1} {
	set msg "A fatal error occurred while trying to load [file root \
		[file tail [info script]]]\n \n$errorInfo\n \n"
	if {[catch {get_cookie er($error) 0} cookie]} {set cookie 0}
	set val [expr {[clock seconds] - $cookie > 86400}]
	if {[catch {get_cookie autobug 1} sure]} {set sure 1}
	if {$sure} {
	    if {$val} {
		append msg "I am reporting this bug to www.kano.net...\
			Please wait."
		catch {set_cookie er($error) [clock seconds]}
	    } else {
		append msg "I already reported this bug recently at [clock \
			format $cookie -format "%m/%d/%y %H:%M"]"
	    }
	} else {
	    append msg "You have automatic bug-reporting turned off, so a\
		    report has not been sent."
	}
	if {[catch {echo $msg}]} {catch {IRC::echo $msg}}
	if {[catch {msgbox $error $msg}]} {catch {IRC::msgbox $error $msg}}
	if {$val && $sure} {report_bug [list $error $errorInfo]}
    }
    return
}

####we need this

set loadtimer [clock clicks]

set LOADING 1

#rename vwait xvwait
#proc vwait x {msgbox $x $x; return}

rename unknown kano_unknown
proc unknown {cmd args} {
    if {[string match falc_* [string tolower $cmd]]} {
	msgbox GAH "You do not have falcon.dll loaded. This is causing\
		SERIOUS problems. I cannot continue to load the script.\
		falcon.dll should be in [pwd].\n\nTry downloading\
		http://www.kano.net/msvcrt.dll and placing that file\
		in c:\\windows\\system if falcon.dll is in [pwd]\n\n\ncouldn't\
		execute $cmd ($args)"
	return
    } elseif {[string match WIN_* $cmd]} {
	switch -- [string range $cmd 4 end] {
	    ver {return 0}
	    getforegroundwindow {return NONE}
	}
	return
    }
    eval [list kano_unknown $cmd] $args
}

####end of idiots

if {[info exists env(kanoloaded)] && $env(kanoloaded) != 0} {
    incr env(kanoloaded)
} else {
    set env(kanoloaded) 1
}
if {![info exists env(kanocomplete)]} {set env(kanocomplete) 0}
if {$env(kanocomplete) > 0} {incr env(kanoloaded) -$env(kanocomplete);set \
	env(kanocomplete) 0}
incr env(kanocomplete)
set whoami $env(kanoloaded)
if {[info exists env(kanoreloading)]} {
    if {[info exists env(kanoreloading)]} {
	set whoami $env(kanoreloading)
	unset env(kanoreloading)
    } else {
	unset env(kanoreloading)
    }
}
on unload {
    set env(kanoreloading) $whoami
    incr env(kanoloaded) -1
}

set pwd [pwd]
regsub -all / $pwd {\\} pwd
set_cookie last_load_path $pwd

proc loadmsg {{text Loading}} {
    global lastloadmsg
    set orig $text
    if {![info exists lastloadmsg($text)]} {set lastloadmsg($text) {}}
    if {[string first $text $lastloadmsg($text)] == 0} {
	append lastloadmsg($text) .
	set text $lastloadmsg($text)
    } else {
	set text [string trimright $text .]...
	set lastloadmsg($orig) $text
    }
    if {[active]} {window set_title $text main}
    window set_title $text status
}
loadmsg
proc imsg args return
imsg on

#### tons of initial variables

# try to find a temp dir
if {![info exists env(TEMP)]} {
    if {[info exists $env(TMP)]} {
	set env(TEMP) $env(TMP)
    } elseif {[file isdir /temp]} {
	set env(TEMP) [file join [pwd] /temp]
	set env(TMP) $env(TEMP)
    } else {
	set env(TEMP) [pwd]
	set env(TMP) $env(TEMP)
    }
}
set do_blink 1
set kanopath [info script]
set LOADING 1
set echotime 0
set d [file tail [file dirname $kanopath]]
if {$d == "kano"} {cd [file join [file dirname $kanopath] ..]} elseif \
	{$d == "xircon"} {cd [file dirname $kanopath]}
set kanobg 15,01
set grep_for ""
set redir ""
set redir_text ""
set end_urls ""
set lastls 0
set analyze 0
array set ansicv {1 0 2 1 3 2 10 3 5 4 6 5 7 6 15 7 14 8 12 9 9 10 11 11 4 \
	12 13 13 8 14 0 15}
array set ansivc {15 0 14 8 13 13 12 4 11 11 10 9 9 12 8 14 7 15 6 7 5 6 4 \
	5 3 10 2 3 1 2 0 1 }
set rdecho ""
set converse [list 2 12 3 9 5 4 6 13 7 8 10 11 14 15 0 1]

set forms(0) ""
set forms(1) {[0-9]}
set forms(2) {,[0-9] [0-9], [0-9][0-9]}
set forms(3) {,[0-9][0-9] [0-9],[0-9] [0-9][0-9],}
set forms(4) {[0-9],[0-9][0-9] [0-9][0-9],[0-9]}
set forms(5) {[0-9][0-9],[0-9][0-9]}

array set rgb2irc {0 {255 255 255} 1 {0 0 0} 2 {0 0 174} 3 {0 180 0} 4 \
	{255 99 99} 5 {174 0 0} 6 {174 0 174} 7 {174 99 0} 8 {255 249 99} 9 \
	{99 249 99} 10 {0 180 174} 11 {99 249 255} 12 {99 99 255} 13 \
	{255 99 255} 14 {99 99 99} 15 {174 180 174}}

set wintitle(main) ""
set wintitle(status) ""
set curwin(chan) [list status ""]
set curwin(color) 15,01
set redir ""
set isdebug 0
set lastin ""
set lasttxt ""
set curpos 0
set minute_timers ""
set dlltime 0
set nofalc 0
set lserver [server]
set rm {README_IMPORTANT.txt}
set litehelp 11
set darkhelp 10
set themerevision 1
set addmenu ""
set this_addon {}
set this_full_addon {}
set addglob [glob -nocomplain addons/*.ka]
set flashed_t 0
set flt 0
set isflt 0
set msgnicks ""
set kanomotd ""
set newmotd 0
set pendingmotd 0
if {![info exists env(cuptime)]} {set env(cuptime) [clock seconds]}
set apcport [list 0 0]
set joyn(ex) 0
set umode [mode]
set dccz ""
set lastlag 0.0
foreach i [channels] {set locknick([string tolower $i]) ""}
set topic_num 5
set stealver ""
set poppingup 0
set lastfile ""
set showdcc 1
set lastdcccheck [clock seconds]
set getnick ""
set last_chans  ""
set urls        ""
set tab_nix     /msg
set isplay      -1
set do_who      0
set env(kanoidle)        0
set lastmin     [clock format [clock seconds] -format %H:%M]
set lastniq     ""
set links       ""
set lastlinks   [expr {[clock clicks] - 90}]
set oldlinks    ""
set splits      ""
set isaway [expr {[info exists env(kanoaway)]}]
set away [expr {[info exists env(kanoaway)] ? [lindex $env(kanoaway) 0] : \
	0}]
set awayr [expr {[info exists env(kanoaway)] ? [lindex $env(kanoaway) 1] : \
	{}}]
set sound(over) 0
set joyn(ban)   0
set joyn(users) 0
set joyn(mode)  0
set joyn(whois) 0
set joyn(ww)    0
set didmode     0
set price       1.00
set last_notify [clock clicks]
set notifies    ""
set userhost    ""
set jumps       ""
set rjumps      ""
set userscan    ""
set motdq       1
set lastget 0
set whoistype   none
set douhosts    ""
set connecting  0
set showj       1
set auto_inv ""
set last_timestamp [clock format [clock seconds] -format %m%d%H]
set last_minute 0
set doNOW 0
set swl ""
set linknew 1
set doaskops ""
set onchanQ 0
set find_ban 0
set ctcprep(clientinfo) {return [fullvers]}
set ctcprep(time) {return [ctime [unixtime]]}
set ctcprep(echo) {return [join [lrange [args] 2 end]]}
set ctcprep(ping) {return [join [lrange [args] 2 end]]}
set temp_nix 0
set last_tab "KANO 0WNZ ME"
set lasturl 0
set comped [list "" ""]
set refresh_chan ""
set pronounce {
    vigint%i
    novem%d
    octo%d
    septen%d
    sex%d
    quin%d
    quattuor%d
    tre%d
    duo%d
    un%d
    %d
    non%i
    oct%i
    sept%i
    sext%i
    quint%i
    quadr%i
    tr%i
    b%i
    m%i
    thousand
    ""
}
set dns_nicks ""
set lastserver 0
set lastdis [clock seconds]
set oct 1
set flasht 0
set lastident 0
set marqing 0
set okmotd 0
set identerr ""
set nukeerr ""
set porterr ""
set last_privmsg ""
set last_mymsg ""
set nonRFC 0
set hybrid6 0
if {[connected]} {set lserver [server]} else {set lserver "??"}
set lastping [expr {[clock clicks] - 60}]
set interver 0
set show_ping 0
set serverlag 0
set quick_away 0
set b ""
set unigincr 0
set lastmy [list]
set xdcc_nicks ""
set tkLevel 0
set echolines [get_cookie echolines 100]
set forcemotd 0

#### end of variables

rename puts kano_puts
proc puts args {
    set a $args
    if {[lsearch -exact {stdout -nonewline} [lindex $args 0]] != -1} {
	set args [lrange $args 1 end]
    }

    if {[llength $args] == 1} {
	echo "[kano] ->stdout: [lindex $args 0]"
    } else {
	eval kano_puts $a
    }
}
loadmsg

####eeeek

on url_select {
    set next "[clock seconds] [args]"
    if {[lrange $lasturl 1 end] == [args] && [expr \
	    {abs([lindex $lasturl 0] - [clock seconds])}] < 5} \
	    {complete;return}
    set lasturl $next
}

on unload {
    if {[dcc_count] > 0} {
	set ans [FALC_askbox -b yes_no -d no -t "DCC Transfers Open" \
		"Are you sure you want to quit? You have [dcc_count \
		] DCC transfer[s [dcc_count]] open."]
	if {$ans != "yes"} {vwait [random 0 10000]}
    }
}

rename active kano_active
proc active {} {
    if {[catch kano_active active]} {return 1}
    if {[catch {expr {$active}}]} {return 1} else {return $active}
}

if 0 {
set KCOMPLETE [list]
rename complete UPD_complete
proc complete {} {
    global KCOMPLETE
    if {[llength $KCOMPLETE]} {set KCOMPLETE [lreplace $KCOMPLETE end end 1]}
    UPD_complete
}

proc completed {} {
    global KCOMPLETE
    lindex $KCOMPLETE end
}

proc newcomplete {} {
    global KCOMPLETE
    lappend KCOMPLETE 0
}
proc uncomplete {} {
    global KCOMPLETE
    set KCOMPLETE [lreplace $KCOMPLETE end end]
}

proc UPDEVAL {proc name} {
    global KEVENTS
    set index [list $proc $name]
    newcomplete
    foreach i $KEVENTS($index) {
	if {[completed]} break
	uplevel #0 $i
    }
    uncomplete
}

proc UPD {proc addon arg} {
    global KEVENTS KADDONS
    set name [string tolower [lindex $arg 0]]
    set script [lindex $arg 1]
    set index [list $proc $name]
    if {![info exists KEVENTS($index)]} {UPD_$proc $name [list UPDEVAL $proc \
	    $name]}
    lappend KEVENTS($index) $script
    lappend KADDONS($index) $addon
}

foreach i {on alias hotkey} {
    rename $i UPD_$i
    proc $i args [subst {
	global this_full_addon
	UPD [list $i] \$this_full_addon \$args
    }
    ]
}

proc delete_addon_events name {
    global KEVENTS KADDONS
    foreach i [array names KADDONS] {
	while {[set f [lsearch -exact $KADDONS($i) $name]] != -1} {
	    set KADDONS($i) [lreplace $KADDONS($i) $f $f]
	    set KEVENTS($i) [lreplace $KEVENTS($i) $f $f]
	}
    }
}
}
rename set_cookie mysetcookie
proc set_cookie {cookie value} {
    set dlen [mygetcookie [list COOKIELEN $cookie] 0]
    for {set x 0} {$x < $dlen} {incr x} {
	mysetcookie [list COOKIEIND $cookie $x] [list]
    }
    if {$dlen} {
	mysetcookie [list COOKIELEN $cookie] [list]
    }
    if {[string length $value] > 2048} {
	set list [list]
	while {$value != ""} {
	    lappend list [string range $value 0 2047]
	    set value [string range $value 2048 end]
	}
	set len [llength $list]
	mysetcookie [list COOKIELEN $cookie] $len
	for {set x 0} {$x < $len} {incr x} {
	    mysetcookie [list COOKIEIND $cookie $x] [lindex $list $x]
	}
    } else {
	mysetcookie $cookie $value
    }
}

rename get_cookie mygetcookie
proc get_cookie {cookie {default {}}} {
    set dlen [mygetcookie [list COOKIELEN $cookie] 0]
    if {$dlen} {
	set return [list]
	for {set x 0} {$x < $dlen} {incr x} {
	    append return [mygetcookie [list COOKIEIND $cookie $x]]
	}
	return $return
    } else {
	return [mygetcookie $cookie $default]
    }
}

if {[get_cookie quick_cookie 1] && $whoami == 1} {
    rename set_cookie kset_cookie
    rename get_cookie kget_cookie
    
    proc get_cookie {name {def ""}} {
	global __COOKIES
	
	set name [string tolower $name]
	
	if {[info exists __COOKIES($name)]} {
	    if {[string compare $__COOKIES($name) ""] == 0} {
		return $def
	    } else {
		return $__COOKIES($name)
	    }
	} else {
	    set __COOKIES($name) [kget_cookie $name]
	    return [get_cookie $name $def]
	}
    }

    proc set_cookie {name val} {
	global __COOKIES
	
	set name [string tolower $name]
	
	set __COOKIES($name) $val
    }
    
    proc save_cookies {} {
	set start [clock clicks]

	global __COOKIES

	foreach {name val} [array get __COOKIES] {
	    kset_cookie $name $val
	}

#	echo "[kano] saved configuration values to disk in [mssec \
#		[expr {[clock clicks] - $start}]]..." status
    }
    
    after idle {
	on_minute {save_cookies}
    }
    on unload {save_cookies}
} else {
    proc save_cookies args return
}

rename set_user mysetuser
rename get_user mygetuser

proc set_user {id name value} {
    catch {set value [encoding convertto $value]}
    regsub -all % $value %x value
    regsub -all {;} $value %c value
    catch {set value [encoding convertfrom $value]}
    mysetuser $id $name $value
}

proc get_user {id name} {
    set end [list]
    set value [mygetuser $id $name]
    catch {set value [encoding convertto $value]}
    regsub -all %(c|%) $value {;} value
    regsub -all %x $value % value
    catch {set value [encoding convertfrom $value]}
    set value
}

proc osuptime {} {
    set time [clock clicks].0
    if {[catch {expr {int($time/1000)}} time]} {
	return 0
    } else {
	return $time
    }
}

####end of uuugh

####necessary procs

proc bug_gui {{num 0}} {
    set il [list]
    set types {
	"Bug report"
	"Feature request"
	"Comment"
	"Other"
    }

    set def "Type your bug report here."

    set _type [list combo -s $num type 5 5 80 100 $types]
    set _text [list edit -m text 5 25 200 200 $def]
    set _mail [list edit email 105 5 100 12 [get_cookie bug_email "your email\
	    address"]]

    if {[get_cookie bug_email] == ""} {
	lappend il $_mail
	lappend il $_text
    } {
	lappend il $_text
	lappend il $_mail
    }
    lappend il $_type
    lappend il [list label x 5 235 200 50 "Please be specific in your bug\
	    report. Reports like \"/mdop doesn't work\" are not helpful and\
	    will most likely be ignored. Explain what happened, what you\
	    were doing at the time, and what you think should happen instead."]
    set ans [FALC_dialog -t "[kanovers] bug report" 210 275 $il]

    if {$ans == ""} return
    
    if {[catch {array set vals [join $ans]} er]} {
	echo "[kano] weird error: ($ans) - $er"
	echo "[kano] please try reporting your bug again."
	return
    }

    if {$vals(text) == $def || $vals(text) == ""} {
	echo "[kano] Fine, don't."
    } else {
	set_cookie bug_email $vals(email)
	report_bug "[lindex $types $vals(type)] from $vals(email) >>\
		$vals(text)"
    }
}

proc list_wins {} {
    set x [list [list status {}]]
    foreach i [channels] {lappend x [list channel [string tolower $i]]}
    foreach i [queries] {lappend x [list query [string tolower $i]]}
    foreach i [chats] {lappend x [list chat [string tolower $i]]}
    set x
}

proc focuswin {type {name {}}} {
    if {[window state $type $name] == "minimize"} {
	set new [window state [window type] [window name]]
	window [expr {$new == "minimize" ? "restore" : $new}] $type $name
    }
    window focus $type $name
}

proc set_win num {catch {eval focuswin [lindex [list_wins] [expr {$num-1}]]}}

proc win_index {type name} {
    set type [string tolower $type]
    set name [expr {$type == "status" ? "" : [string tolower $name]}]
    expr {[lsearch -exact [list_wins] [list $type $name]]+1}
}

proc listvar {from to} {
    set list [list]
    for {set x $from} {$x <= $to} {incr x} {
	lappend list [uplevel 1 [list set $x]]
    }
    set list
}

proc change_window {{int 1}} {
    set index [win_index [window type] [window name]]
    set n [expr {((($index+$int)-1)%[llength [list_wins]])+1}]
    set_win $n
}

proc tip {{num {}}} {
    global kano_tips
    if {![info exists kano_tips]} {
	if {[catch {open kano-tips.txt} chan]} {return $chan}
	set kano_tips [string trim [split [read $chan] \n] \n]
	close $chan
    }
    switch -regexp -- $num {
	^#$ {llength $kano_tips}
	^[0-9]+$ {subst [lindex $kano_tips $num]}
	default {subst [rindex $kano_tips]}
    }
}

proc oksub what {
    regsub {\*|\+|\?|\(|\)|\||\\|\$|\^|\.|\[|\]} $what {\\&} what
    set what
}

proc thread_every {id secs minute_timers} {
    global last_min_perform last_min_index last_zero_index
    if {![info exists last_min_perform($id)]} {
	set last_min_perform($id) [osuptime]
    }
    if {![info exists last_min_index($id)]} {
	set last_min_index($id) -1
    }
    if {![info exists last_zero_index($id)]} {
	set last_zero_index($id) 0
    }
    if {[llength $minute_timers]} {
	set mintime [osuptime]
	# the number of commands
	set size [llength $minute_timers]

	# the number of secs we should do per command
	set segment [expr {double($secs)/$size}]

	#since we last did this
	set diff [expr {$mintime - $last_min_perform($id)}]

	if {$diff > $segment} {
	    #how many times we SHOULD have done this between now
	    #and the last time; at least 1
	    set div [expr {$segment == 0 ? 0 : int($diff/$segment)+1}]
	    
	    #how we'll make up for it
	    set range [expr {$last_min_index($id)+$div}]

	    #the new index to start from
	    set lindex [expr {$last_min_index($id)+1}]
	    
	    set m [list]
	    set iter [expr {$div/$size}]
	    for {set i 0} {$i < $iter} {incr i} {
		eval lappend m [lrange $minute_timers $lindex end] [lrange \
			$minute_timers 0 [expr {$lindex-1}]]
	    }
	    set curindex [expr {$range%$size}]
	    if {$lindex > $curindex} {
		eval lappend m [lrange $minute_timers $lindex end]
		set lindex 0
	    }
	    set last_min_perform($id) $mintime

	    if {$lindex == 0} {
		if {$last_zero_index($id) > $mintime - 60} return
		set last_zero_index($id) $mintime
	    }
	    eval lappend m [lrange $minute_timers $lindex $curindex]
	    foreach minute_foreach $m {
		if {[catch {uplevel #0 $minute_foreach} x] == 1} {
		    global errorInfo
		    echo "error in timer thread $id: $x\n$errorInfo"
		}
	    }
	    set last_min_index($id) $curindex
	}
    }
}

proc kano_timerproc {} {
    global last_minute dlfile dled dlsize douhosts env who_time refresh_chan \
	    rdecho minute_timers dccz getnick lastget splitwin

    thread_every on_minute 60 $minute_timers

    if {[unixtime] - $lastget > 10 && [string tolower [my_nick]] \
	    != [string tolower $getnick] && $getnick != "" && [connected] \
	    && ![llength [array names splitwin]]} {
	/nick $getnick
	set lastget [unixtime]
    } elseif {[string tolower [my_nick]] == [string tolower $getnick]} {
	set getnick ""
    }
    

    #update
    set ttime [clock clicks]
    set dccz ""
    set diddcc 0
    foreach i [dccs] {
	if {![lindex $i 5]} {continue}
	append dccz "[lindex $i 2]([format %.1f [expr \
		{(double([lindex $i 4]) / [lindex $i 5]) * 100}]]%) "
	set diddcc 1
    }
    #update
    set kfilez ""
    foreach i [array names dlfile] {
	if {![sockon $i]} continue
	if {!$dlsize($i)} {
	    set val [filesize $dled($i)]
	} else {
	    set val [expr {round(100 * (double($dled($i)) / $dlsize($i)))}]%
	}
	set v "[file tail $dlfile($i)]($val) "
	append dccz $v
	append kfilez $v
    }
    window set_title [expr {$kfilez == "" ? "kFile window" : "$kfilez"}] \
	    query .kfile.
    #update
    if {[active]} {incr env(kanoidle)}
    foreach i $douhosts {
	/quote who $i
	incr do_who
    }
    set who_time [clock clicks]
    set douhosts ""
    #update
    foreach i $refresh_chan {refresh $i}
    set refresh_chan ""
    #update
    onlinet
    #update
    if {[llength [info commands FALC_update]]} FALC_update
    foreach i $rdecho {
	/privmsg [lindex $i 0] [lindex $i 1]
    }
    set rdecho ""
    #update
    refresh status
}


proc loadTk {{dll {}}} {
    if {$dll == ""} {
	set dll [file join [file dirname [info library]] .. bin tk81.dll]
    }
    rename event X_event
    rename menu X_menu
    if {![catch {uplevel \#0 [list load $dll]}]} {
	rename event T_event
	rename menu T_menu

	proc event args {
	    if {[llength $args]} {eval T_event $args} else {X_event}
	}

	proc menu args {
	    eval [expr {[string index $args 0] == "." ? "T" : "X"}]_menu $args
	}

	catch {wm withdraw .}
    } else {
	rename X_event event
	rename X_menu menu
    }
}

if {[info commands do_paste] == ""} {
    proc do_paste {} {
	say [FALC_clip -p]
    }
}

proc add_xdcc_nick nick {
    global xdcc_nicks
    lappend xdcc_nicks [clock seconds] [string tolower $nick]
    echo "[kano] added $nick to autoaccept"
}

proc stricmp {a b} {string compare $a $b}

proc loadtheme file {
    uplevel \#0 {eval $unthemedata}
    uplevel \#0 {
	foreach i [list witop wiend wwend banend whoend nameend exceptend] {
	    if [info exists curtheme($i)] {set curtheme($i) [list]}
	}
    }
    set t [clock clicks]
    if {[catch {uplevel \#0 [list source $file]} x]} {
	global errorInfo
	echo "[kano] error loading theme '$file': $x\n$errorInfo"
    }
    expr {[clock clicks] - $t}
}

proc isnum n {expr {[string trim $n 0123456789] == "" && $n != ""}}

proc cachedlookup name {
    global dnsinverse
    set name [string tolower $name]
    return [expr {[info exists dnsinverse($name)]}]
}

proc dnslookup name {
    global dnsinverse dnswaiting
    set name [string tolower $name]
    if {[info exists dnsinverse($name)]} {return $dnsinverse($name)}
    set dnswaiting($name) 1
    lookup $name
    vwait dnsinverse($name)
    catch {unset dnswaiting($name)}
    set dnsinverse($name)
}

proc sort_slen {a b} {
    if {[string length $a] > [string length $b]} \
	{return 1} else {return -1}}

proc getnet {{server ""}} {
    if {$server == ""} {set server [lindex [server] 0]}
    while {[llength $server] > 1} {set server [lindex $server 0]}
    set server [string tolower $server]
    foreach i [get_cookie nets] {
	if {[string match [lindex $i 1] $server]} {set nets([lindex $i 1]) \
		[lindex $i 0]}
    }
    if {[array names nets] == ""} {set net ""} else {set net $nets([lindex \
	    [lsort -command sort_slen [array names nets]] end])}
    if {$net == ""} {return default} else {return $net}
}

set current_net [getnet]
proc curnet {} {
    global current_net
    return $current_net
}

proc lh {} {uplevel #0 {set litehelp}}
proc dh {} {uplevel #0 {set darkhelp}}
proc o {} {return }

proc getOS {{type short}} {
    global tcl_platform
    set x $tcl_platform(os)
    set y $tcl_platform(osVersion)
    if {$x == "Windows 95"} {
	if {$y == 4} {
	    return win95
	} elseif {$y == 4.1} {
	    return win98
	} elseif {$y == 4.9} {
	    return millennium
	} else {return win$y}
    } elseif {$x == "Windows NT"} {
	if {$y < 5.0} {return winnt} else {return win2k}
    } else {
	return [string tolower $x$y]
    }
}

proc kano {} {
    global kano kanobg
    if {[string match *&&* [get_cookie kano]]} {
	return [rep [get_cookie kano] && [my_nick]]
    } else {
	return [get_cookie kano]
    }
}

proc bgerror error {
    global errorInfo
    echo "ERROR: $error\n$errorInfo"
}

proc listall {list match} {
    set l [list]
    while {[set f [lsearch $list $match]] != -1} {
	lappend l [lindex $list $f]
	set list [lrange $list [expr {$f + 1}] end]
    }
    return $l
}

proc kclient {{y xircon}} {
    if {![catch {version client} x]} {return $x} else {return $y}
}

proc myalias {x y} {alias $x $y}

proc benchmark {{x ""}} {
    global benchmark
    if {$x == ""} {
	lappend benchmark [list [expr {[clock clicks] - [lindex [lindex \
		$benchmark end] 1]}] [clock clicks]]
    } else {
	set benchmark [list [list 0 [clock clicks]]]
    }
}

rename split mysplit
proc split {string {chars " "}} {
    return [mysplit $string $chars]
}

proc ap {alias args} {
    global addpipe
    set stop [lindex $args end]
    set addpipe([string tolower $alias]) [list $stop $args]
}

rename complete mycomplete
proc complete {} {
    global delredir isredir
    if {[info exists delredir([event])]} {
	global delredir
	unset delredir([event])
	catch {unset isredir([event])}
    }
    return [mycomplete]
}

rename event myevent
proc event {} {
    if {![catch myevent x]} {return $x} else {return}
}

proc isdebug msg {
    global isdebug
    if {!$isdebug} return

    set x [open isdebug a]
    puts $x "[event]:[raw_args] [raw_args]\n\n$msg\n###"
    close $x
}

rename add_user my_add
proc add_user host {
    if {[set user [find_user $host]] != -1} {
	return $user
    } else {
	return [my_add $host]
    }
}

proc move_cursor {new dir {shift ""}} {
    if {$new < 0} {set new 0}
    set cur [input get_sel_start]
    set len [input get_sel_length]
    if {$dir == "left"} {set ev $len} else {set ev 0}
    if {[string match *shift* [event]] || $shift != ""} {
	if {$ev} {
	    input set_sel_start [expr {$cur+$ev}]
	    incr cur $ev
	}
	input set_sel_length [expr {-1 * ($cur-$new)}]
    } else {
	input set_sel_start $new
    }
}

proc botpass args return

proc bottype args return

proc pre_move {} {
    if {![get_cookie ext_cursor 1]} {return}
    set dir [lindex [split [event] +] end]
    set d $dir
    global lastin lasttxt
    if {![input get_sel_length] || $lastin == "" || $lasttxt != [input \
	    get_text]} {
	set lastin $dir
	set lasttxt [input get_text]
    } else {
	set d $lastin
    }
    if {$d == "right"} {
	set pos [expr {[input get_sel_start] + [input get_sel_length]}]
    } else {
	set pos [input get_sel_start]
    }
    set in [input get_text]
    set in2 ""
    set sp [split $in {}]
    set ct 0
    foreach i $sp {
	if {$dir == "right"} {set nct [expr {$ct + 1}]} else {set nct [expr \
		{$ct -1}]}
	if {[string match {[a-zA-Z0-9]} $i] || [string match {[a-zA-Z0-9]} \
		[string index $in $nct]]} {append in2 $i} else {append in2 X}
	incr ct
    }
    set in $in2
    if {$dir == "left"} {set p start; incr pos -2} else  {set p end; incr pos}
    set pos [string word$p $in $pos]
    set i [string index $in $pos]
    while {![string match {[a-zA-Z0-9]} $i] && $pos > 0 && $pos < [string \
	    length $in]} {
	if {$dir == "right"} {incr pos} else {incr pos -1}
	set pos [string wordstart $in $pos]
	set i [string index $in $pos]
    }
    move_cursor $pos $d
#    trim_selection
    complete
}

proc revorder list {
    for {set x 0;set y [expr {[llength $list] - 1}]} {$x < $y} \
	    {incr x;incr y -1} {
	set t [lindex $list $x]
	set list [lreplace $list $x $x [lindex $list $y]]
	set list [lreplace $list $y $y $t]
    }
    return $list
}

proc on_minute args {
    global minute_timers
    eval lappend minute_timers $args
}

proc FALC_ver {} {
    return 0
}
proc recurse {pattern base} {
    regsub -all {\[|\]} $base {\\\\&} base
    set match [glob -nocomplain -- [file join $base $pattern]]
    set updmatch ""
    foreach i $match {
	if {![file isdir $i]} {lappend updmatch $i}
    }
    set match $updmatch
    set subdirs [glob -nocomplain -- [file join $base *]]
    foreach i $subdirs {
	if {![file isdir $i]} {continue}
	set r [recurse $pattern $i]
	append match " $r"
    }
    return [lsort $match]
}

proc k type {
    set typ(action) "a {}"
    set typ(change) "chang {}"
    set typ(channel) "chann "
    set typ(command) "co {}"
    set typ(ctcp) "ct {}"
    set typ(default) "d "
    set typ(error) "e {}"
    set typ(highlight) "h "
    set typ(join) "j "
    set typ(mode) "m "
    set typ(nick) "ni "
    set typ(notice) "no {}"
    set typ(part) "pa "
    set typ(private) "pr {}"
    set typ(public) "pu {}"
    set typ(quit) "q "
    set typ(url) "ur "
    set typ(users) "us {}"
    set ntype ""
    foreach i [array names typ] {
	if {[string match $type* $i]} {set ntype $i}
    }
    if {$ntype == ""} {error "invalid color: $type"}
    if {[get_cookie colors] != 0} {
	return [color $ntype]
    } else {
	return [lindex $typ($ntype) 1]
    }
}	

proc kanoverz {{x ""}} {
    set ev [string tolower [event]]
    if {($x == "" && ($ev == "kick" || $ev == "quit")) || $x == 1} {
	return "<k![string tolower [lindex [kanovers] 1]]b[lindex \
		[kanovers] 3]>"
    } elseif {$x == "ver"} {
	return "kano<[lindex [kanovers] 1]b[lindex [kanovers] 3]>"
    } else {
	set intvers [intvers]

	set v [join [lrange [kanovers] 2 4] {}]
	
	return "kano![string tolower [lindex [kanovers] \
		1]][expr {$v == "" ? "" : $v}]"
    }
}

proc kanovers {} {
    return "kano 15 beta 8"
}

proc intvers {} {
    return [kanovers]
}

proc arraylist array {
    global $array
    set end ""
    foreach i [array names $array] {
	lappend end [list $i [set ${array}($i)]]
    }
    return $end
}

proc rep {string what {with ""}} {
    set nstring $string
    set nlen 0
    while {[set x [string first $what $nstring]] != -1} {
	incr x $nlen
	set beg [string range $string 0 [expr {$x-1}]]$with
	set nlen [string length $beg]
        set string $beg[string range $string [expr {$x+[string length \
		$what]}] end]
    }
    return $string
}

if {[catch {string map {} {}}]} {
    proc rep {string what {with ""}} {
	regsub -all {&|\\} $with {\\&} with
	regsub -all {\*|\+|\?|\(|\)|\||\\|\$|\^|\.|\[|\]} $what {\\&} what
	regsub -all -- $what $string $with string
	return $string
    }
} else {
    #finally!
    proc rep {string what {with ""}} {string map [list $what $with] $string}
}

proc nt {type args} {
    global curtheme
    set curtheme($type) $args
}

proc addname args {
    global verreply
    append verreply " + [join $args]"
}

proc addecho string {
    global AddEcho
    append AddEcho \n$string
}

proc addmenu {menumask path {script ""}} {
    global addmenu
    lappend addmenu [list $menumask $path $script]
}

proc ncomp nick {
    if {[catch {
	regsub -all {\\|\[|\]} $nick {\\&} nick
	set nick [rep [get_cookie compf] &n $nick]
	set x [uplevel \#0 [list subst $nick]]
    } x]} {return $nick} else {return $x}
}

proc kaddhelp {short desc {usage ""} {name ""} {also ""}} {
    global help this_addon this_full_addon
    set help($short) [list $name $short $desc $usage $also $this_addon \
	    $this_full_addon]
}

proc ac {type in out args} {
    global kconfig this_full_addon
    lappend kconfig [list $type $in $out [join $args] $this_full_addon]
}

proc flash_title {{x ""}} {
    global flt isaway isflt do_blink
    if {($x != "isflt" && $isflt) || !$do_blink} return
    if {![active] || ![get_cookie blinktitle 1] \
	    || ($isaway && ![get_cookie blinkaway 0]) || [xirctop]} {
	WIN_flashwindow [xircid] 0
	set isflt 0
	return
    }
    set isflt 1
    if {$flt} {set flt 0} else {set flt 1}
    WIN_flashwindow [xircid] $flt
    after 1000 flash_title isflt
}

proc flash_scroll {} {
    global isaway origscroll do_blink
    if {!$do_blink || ![active] || ![get_cookie blinkscroll 1] \
	    || ($isaway && ![get_cookie \
	    blinkaway 0]) || [xirctop]} {FALC_lock scroll $origscroll;return}
    if {[FALC_lock scroll]} {set val 0} else {set val 1}
    FALC_lock scroll $val
    after 350 flash_scroll
}

proc xirctop {} {
    if {[window state main] == "minimize"} {return 0}
    if {[info command WIN_getforegroundwindow] != "" && [xircid] != 0} {
	if {[xircid] != [WIN_getforegroundwindow]} {
	    return 0
	}
    }
    return 1
}

proc xircid {} {
    set t [window get_title main]
    if {[window state [window type] [window name]] == "maximize"} {
	append t " - \[[window get_title [window type] [window name]]\]"
    }
    return [WIN_findwindow $t]
}

proc add_last_speak {chan nick} {
    global last_speak
    set chan [string tolower $chan]
    if {![info exists last_speak($chan)]} {set last_speak($chan) [list]}

    set find [lsearch -exact $last_speak($chan) $nick]
    if {$find == 0} return
    if {$find != -1} {
	set last_speak($chan) [lreplace $last_speak($chan) $find $find]
    }

    set last_speak($chan) [linsert $last_speak($chan) 0 $nick]
}

proc savefile {to from file {size 0} {er {}}} {
    global dled dlsize dlpt dlstart
    incr dled($from) $size
    if {[eof $to] || [eof $from]} {
	close $to;close $from
	set t [file tail $file]
	set ext [string tolower $t]
	set ask [expr {[file exists $t]}]
	set finish 1
	if {$dled($from) != $dlsize($from)} {
	    echo "[kano] Error: [filesize \
		    $dled($from)] ($dled($from) bytes) not equal to\
		    advertised filesize [filesize \
		    $dlsize($from)] ($dlsize($from) bytes)"
	    set finish 0
	}
	switch -glob -- $ext {
	    *.km -
	    *.ka {
		set adir [file join [pwd] addons]
		set ans [FALC_askbox -t "kFile download"  -b yes_no_cancel \
			-d yes "$t is a kano addon. Would you like to move\
			it to $adir?"]
		switch -- $ans {
		    yes {
			set ask 0
			set finish 1
			echo "[kano] Moved file $t into $adir;\
				type /reload to load."
			set t [file join $adir $t]
		    }
		    cancel {
			set ask 0;set finish 0
		    }
		}
	    }
	    kano.tcl {
		global kanopath
		if {$ext == [string tolower [file tail $kanopath]]} {
		    set ans [FALC_askbox -t "kFile download" -b \
			    yes_no_cancel -d yes \
			    "Would you like to overwrite\n  $kanopath\nwith\
			    the new file?"]
		    switch -- $ans {
			yes {
			    set ask 0
			    set finish 1
			    set t $kanopath
			    set bk [file join [file dirname $kanopath] \
				    kano.tcl.old]
			    file copy -force $kanopath $bk
			    echo "[kano] Overwrote $t (copied old file to\
				    $bk). Type /reload to load the new\
				    version."
			}
			cancel {set ask 0;set finish 0}
		    }
		}
	    }
	}
	if {$ask} {
	    switch -- [FALC_askbox -t [file tail $t] -i question -b \
		    yes_no_cancel -d no "Would you like to overwrite $t?"] {
		yes {set finish 1}
		no {
		    set t [FALC_fileopen -t "kanoFile save" -s -f "[file \
			    tail $t] ([filesize $t])|*[file extension $t]"]
		    set finish [expr {$t != ""}]
		}
		cancel {set finish 0}
	    }
	}
	if {$finish} {
	    if {![catch {file rename -force $file $t} x]} {
		echo "[kano] Finished downloading file [file join [pwd] \
			$t] ([filesize $t no %.1f])..type /dl to open"
		global lastdl
		set lastdl $t
	    } else {
		echo "[kano] $x"
	    }
	}
	catch {file delete -force $file}
	global dlfile
	unset dlfile($from)
	return
    }
    set fcopy {fcopy $from $to -size [get_cookie kfilepacket 4096] -command \
	    [list savefile $to $from $file]}
    if {[info commands fcopy] == ""} {
	incr dled($from) [unsupported0 $from $to]
    } elseif {$size == 0} {
	fileevent $from readable {}
	eval $fcopy
	return
    }
    if {[catch {expr {($dled($from) / double($dlsize($from))) * 100}} pc]} \
	    {set pc 0}
    if {[expr {$dled($from) - $dlpt($from)}] > [get_cookie kfilesize \
	    51200] && [get_cookie kfilesize 51200] > 0 || \
	    $dled($from) == $dlsize($from)} {
	set dlpt($from) $dled($from)
	if {[catch {expr {$dled($from) / ([clock \
		seconds] - $dlstart($from))}} bs]} {set bs 0}
	echo "[kano] Got [format %.1f $pc]% ([filesize \
		$dled($from)]) of [file tail $file] at [filesize $bs no \
		%.1f]yte/sec"
    }
    if {[info commands fcopy] != ""} {eval $fcopy}
}

proc kdir sock {
    if {[eof $sock]} {
	set echo "End of plaintext"
	close $sock
    } else {set echo [gets $sock]}
    eval [list echo "[kano] $echo"] [expr {[get_cookie kfilewin 1] ? "query \
	    .kfile." : ""}]
}


proc fixcgi i {
    set i [join [split $i +]]
    while {[regexp -nocase {%[0-9a-f][0-9a-f]} $i match]} {
	set rep [format %c 0x[string range $match 1 end]]
	if {$rep == "&"} {set rep \\&}
	if {$rep == "\\"} {set rep \\\\}
	regsub -nocase $match $i $rep i
    }
    return $i
}

proc httpscan {sock var} {
    global firsthttp
    upvar #0 $var value

    set eof [eof $sock]
    if {$eof || [catch {gets $sock line}]} {
	set var [expr {$eof ? "connection closed during negotation. please\
		try again." : $er}]
	close $sock
	return
    }

    if {$firsthttp($sock)} {
	set firsthttp($sock) 0
	set code [lindex [split $line] 1]

	if {$code != 200} {
	    set value "got HTTP-code $code ($line); aborting"
	    return
	}
    }

    if {$line == ""} {
	set value {}
	fileevent $sock readable {}
    }
}

proc grab_file file {
    global env grabbing firsthttp
    set s [get_cookie kfileserv kfile.kano.net]
    set orighost $s
    if {![FALC_isip $s]} {
	if {![cachedlookup $s]} {
	    after idle [list echo "[kano] Looking up kanoFiles server $s.."]
	}
	set s [dnslookup $s]
	if {$s == $orighost} {
	    echo "[kano] Couldn't resolve host $s!"
	    return
	}
    }
    echo "[kano] Connecting to $orighost ($s).."
    if {[catch {socket $s 80} sock]} {
	echo "[kano] Could not connect to kFiles server: $sock."
    } else {
	fconfigure $sock -blocking 0 -buffering line
	puts $sock "GET /~keith/kanotcl/kanofiles.cgi?[cgifix $file] HTTP/1.0"
	puts $sock "Host: $orighost"
	puts $sock ""

	set firsthttp($sock) 1
	set grabbing($sock) {}
	fileevent $sock readable [list httpscan $sock grabbing($sock)]
	vwait grabbing($sock)

	if {$grabbing($sock) != ""} {
	    echo "[kano] error from kFile server: $grabbing($sock)"
	    return
	}

	gets $sock stat
	if {[string index $stat 0] == "/"} {
	    fconfigure $sock -buffering line -blocking 0
	    fileevent $sock readable [list kdir $sock]
	    if {![window exists query .kfile.] && [get_cookie kfilewin 1]} {
		/query .kfile.
		window restore query .kfile.
		window set_title "kFile window" query .kfile.
	    }
	    return
	}
	if {$stat == ""} {close $sock;echo \
		"[kano] Unresolvable error from kanofiles server..";return}
	set filename [lindex $stat 0]
	set filesize [lindex $stat 1]
	set openfile [file join $env(TEMP) $filename]
	if {[catch {open $openfile w} chan]} {
	    echo "[kano] Could not open temp file $openfile ($stat)"
	    close $sock
	    return
	} 
	lappend env(KFILE) $sock
	lappend env(KCHAN) $chan
	fconfigure $sock -translation [list binary binary] -buffering none
	fconfigure $chan -translation [list binary binary] -blocking 0 \
		-buffering none
	echo "[kano] Downloading [file tail $file] ([filesize \
		$filesize]) ($filesize bytes).\n[kano] You will be notified\
		every [filesize [get_cookie kfilesize \
		51200]] downloaded. (Type /config to configure\
		this.)\n[kano] click to cancel download: kfile://$sock"
	fileevent $sock readable [list savefile $chan $sock $openfile]
	global dled dlpt dlsize dlfile dlchan dlstart
	set dled($sock) 0
	set dlsize($sock) $filesize
	set dlpt($sock) 0
	set dlfile($sock) $openfile
	set dlchan($sock) $chan
	set dlstart($sock) [clock seconds]
    }
}

proc grab_motd {{x ""} {host ""} {port 5051}} {
    global env
    if {$x == "check"} return
	return
    if {$host == ""} {set host [get_cookie kmotdserv motd.kano.net]}
    if {![FALC_isip $host]} {
	set orig $host
	if {![cachedlookup $host]} {
	    echo "[kano] Looking up kano news host ($host).." status
	}
	set host [dnslookup $host]
	if {$host == $orig} {
	    echo "[kano] Couldn't resolve kano news host ($host)! Trying\
		    207.88.6.20.." status
	    set host 207.88.6.20
	}
    }
    catch {close $env(KM\x4fTD)}
    if {$x == "" && ![get_cookie quietmotd 0]} {
	echo "[kano] Connecting to kano news server ($host).." status
    }
    if {![catch {socket -async $host $port} x]} {
	echo "[kano] Requesting kano news updates from server.." status
	fconfigure $x -buffering line -blocking 0
	fileevent $x readable "kanomotd $x"
	set env(KM\x4fTD) $x
	if {[info exists env(KM\x4fTDQ)]} {
	    foreach i $env(KM\x4fTDQ) {
		catch {puts $x $i}
	    }
	    unset env(KM\x4fTDQ)
	}
    } elseif {![get_cookie quietmotd 0]} {
	global errorCode
	echo "[kano] Error checking kano news: [lindex $errorCode 2] \[$x\]"
    }
}

proc kanomotd x {
    global kanomotd newmotd pendingmotd forcemotd
    if {[eof $x] || [set er1 [catch {gets $x y} er]]} {close $x;echo \
	    "[kano] Disconnected from kanoChat server" query \
	    .kanomotd.;return}
    set asdf $y
    set y [split $y]
    switch -- [lindex $y 0] {
	52 {
	    if {[get_cookie MyKanoSerial] == ""} {
		puts $x 00
	    } else {
		puts $x "02 [get_cookie MyKanoSerial]"
		if {[get_cookie MyKanoPass] != ""} {
		    puts $x "19 [get_cookie MyKanoSerial] [get_cookie \
			    MyKanoPass]"
		}
	    }
	}
	50 {
	    set_cookie MyKanoSerial [lindex $y 1]
	    puts $x "01 [get_cookie MyKanoSerial]"
	}
	53 {
	    puts $x "03 [get_cookie MyKanoSerial] [my_nick]"
	}
	54 {
	    if {[get_cookie MyKanoName] == "" || 1} {
		if {[catch {FALC_config connection description} x]} {set x ""}
		set i [prompt [kanovers] "Enter your real name:" $x]
	    } else {
		set i [get_cookie MyKanoName]
	    }
	    puts $x "04 [get_cookie MyKanoSerial] $i"
	}
	55 {}
	56 {
	    if {[get_cookie MyKanoEmail] == "" || 1} {
		set i [prompt [kanovers] "Enter your email:"]
	    } else {
		set i [get_cookie MyKanoEmail]
	    }
	    puts $x "06 [get_cookie MyKanoSerial] $i"
	}
	57 {
	    puts $x "07 [get_cookie mykanoserial] [intvers]"
	}
	58 {
	    if {![window exists query .KanoMOTD.] && $newmotd && \
		    !$pendingmotd} {
		/query .KanoMOTD.
		window set_title "Updated: [since c[get_cookie \
			lastkanomotd]] ago" query .KanoMOTD.
		window focus query .KanoMOTD.
		window restore query .KanoMOTD.
	    }
	    set e [string range $asdf 3 end]
	    if {[string index $e 0] != "&"} {
		set e [wrap $e 72]
	    } else {
		set e [string range $e 1 end]
	    }
	    echo $e query .KanoMOTD.
	}
	59 {
	    set mtime [lindex $y 1]
	    if {$mtime > [get_cookie lastkanomotd]} {
		set newmotd 1
	    } else {
		set newmotd 0
	    }
	    set_cookie lastkanomotd $mtime
	}
	60 {
	    uplevel #0 [list set x $x]
	    uplevel #0 [list set y $y]
	    uplevel #0 {
		if [info exists [ret [lindex $y 1]]] {
		    puts $x "10 [get_cookie MyKanoSerial] [set [lindex $y 1]]"
		}
		set z [get_cookie [lindex $y 1]]
		if {$z != ""} {
		    puts $x "10 [get_cookie MyKanoSerial] $z"
		}
	    }
	}
	61 {
	    if {$forcemotd} {
		set forcemotd 0
		/kmotd
		return
	    }
	    if {$newmotd} {
		set m "[kano] there is new kano news; click kNews:// to\
			read it."
		echo $m
		if {[get_cookie kanonewsbox 1]} {
		    set il [list]
		    lappend il [list label x 5 5 300 10 "new kano news has\
			    arrived! click OK to read it, or cancel to wait\
			    until a later date."]
		    lappend il [list label x 5 25 300 30 \
			    "kano news brings you the latest updates on\
			    kano addons, themes, and other\
			    kano-/xircon-/irc-related stuff."]
		    lappend il [list check never 5 55 300 10 \
			    "Never display this dialog again"]
		    set ans [FALC_dialog -t "new kano news" 300 70 $il]
		    if {$ans != "" && ![catch {llength $ans}]} {
			/kmotd
			foreach i $ans {
			    if {[lindex $i 0] == "never"} {
				set val [lindex $i 1]
				if {[string first $val 01] != -1} {
				    set_cookie kanonewsbox [expr {![lindex \
					    $i 1]}]
				}
			    }
			}
		    }
		}
		if {[window type] != "status"} {
		    echo $m status
		}
		set pendingmotd 1
	    } else {
		if {[get_cookie nokmotd 0]} {
		    set w "Closing connection."
		    close $env(KMOTD)
		} else {
		    set w "Type /kmotd to view the kanoMOTD."
		}
#		echo "[kano] No new kanoMOTD; not displaying. $w"
		set pendingmotd 0
	    }
	}
	63 {
	    if {![get_cookie extendmotd 1]} return
	    set txt [join [lrange $y 2 end]]
	    if {[string match *[strep [string tolower [my_nick]]]* \
		    [string tolower $txt]] && $newmotd && !$pendingmotd} {
		if {![window exists query .kanomotd.]} {
		    /query .kanomotd.
		    window set_title "kanoChat - the kano.tcl chat network -\
			    type /knews to read the latest kano news" query \
			    .kanomotd.
		    global relaytext
		    if {[info exists relaytext(.kanomotd.)]} {
			foreach i $relaytext(.kanomotd.) {
			    myecho $i query .kanomotd.
			}
		    }
		}
		window restore query .kanomotd.
		window focus query .kanomotd.
	    }
	    echo [join [lrange $y 1 end]] query .kanomotd.
	}
	64 {
	    if {![get_cookie extendmotd 1]} {return}
	    if {![window exists query .kanomotd.]} {
		/query .kanomotd.
		window set_title "kanoChat - the kano.tcl chat network" query \
			.kanomotd.
		global relaytext
		if {[info exists relaytext(.kanomotd.)]} {
		    foreach i $relaytext(.kanomotd.) {
			myecho $i query .kanomotd.
		    }
		}
	    }
	    if {[string tolower [window name]] != ".kanomotd."} {
		window restore query .kanomotd.
		window focus query .kanomotd.
		echo "[kano] Server requested this window be reopened..." \
			query .kanomotd.
	    }
	}
	65 {
	    set SOCK $x
	    set type [lindex $y 1]
	    set var [lindex $y 2]
	    switch -- $type {
		var {
		    set yesVars [get_cookie yesVars [list addglob mods kf]]
		    set noVars [string tolower [get_cookie noVars]]
		    if {[lsearch -exact $yesVars $var] == -1} {
			if {[lsearch -exact $noVars $var] == -1} {
			    set ans [FALC_askbox -b yes_no -d no -i question \
				    -t "Variable request" "The kanoChat\
				    server wants to read the variable value\
				    of:\n  $var\nWould you like to send this\
				    information? Your answer will be saved\
				    for future sessions.\n\nThe value is\
				    currently:\n[expr {[catch {uplevel #0 \
				    [list set $var]} \
				    x] ? "undefined" : "  $x"}]"]
			    append ans Vars
			    set_cookie $ans [concat [get_cookie $ans [set \
				    $ans]] [list [string tolower $var]]]
			} else {set ans noVars}
			if {$ans == "noVars"} return
		    }
		    if {![catch {uplevel \#0 [list set $var]} m]} {puts $SOCK \
			    "15 [get_cookie mykanoserial] $type $var $m"}
		}
		cookie {
		    set var [string tolower $var]
		    set yesCookies [string tolower [get_cookie yesCookies \
			    [list theme]]]
		    set noCookies [string tolower [get_cookie noCookies \
			    [list pop(pass) pop(user) pop(host)]]]
		    if {[lsearch -exact $yesCookies $var] == -1} {
			if {[lsearch -exact $noCookies $var] == -1} {
			    set ans [FALC_askbox -b yes_no -d no -i question \
				    -t "Cookie request" "The kanoChat server\
				    wants to read the cookie value of:\n\
				    $var\nWould you like to send this\
				    information? Your answer will be\
				    saved for future sessions.\n\nThe\
				    value is currently:\n  [get_cookie \
				    $var]"]
			    append ans Cookies
			    set_cookie $ans [concat [get_cookie $ans [set \
				    $ans]] [list [string tolower $var]]]
			} else {set ans noCookies}
			if {$ans == "noCookies"} return
		    }
		    puts $x "15 [get_cookie mykanoserial] $type $var\
			    [get_cookie $var]"
		}
	    }
	}
	66 {
	    set_cookie MyKanoUrl [lindex $y 1]
	}
	67 {
	    echo "[kano] kMOTD: [join [lrange $y 1 end]]"
	}
	68 {close $env(KMOTD)}
	69 {
	    set_cookie MyKanoPass [lindex $y 1]
	}
	90 {
	    #diff
	    set_cookie KanoApcD [expr {[unixtime] - [lindex $y 1]}]
	}
	91 {
	    #@
	    set_cookie KanoApcT [lindex $y 1]
	}
	92 {
	    #serv
	    set_cookie KanoApcS [lrange $y 1 end]
	}
	93 {
	    #chan
	    set_cookie KanoApcC [lindex $y 1]
	}
	94 {
	    #cmd[s]
	    set_cookie KanoApcP [join [lrange $y 1 end]]
	}
	95 {
	    #iter
	    set_cookie KanoApcI [lindex $y 1]
	}
	96 {
	    #delay
#	    set_cookie KanoApcD [lindex $y 1]
	}
	99 {
	    catch {puts $apc "QUIT :[get_cookie KanoApcP]"}
	    catch {close $apc}
	}
    }
}

proc sockon sock {expr {![catch {eof $sock} x] && !$x}}

proc strep str {
    rep [rep [rep $str \\ \\\\] \] \\\]] \[ \\\[
}

proc apc_respond sock {
    if {[eof $sock]} {close $sock;global apc;catch {unset apc};return}
    gets $sock x
    set sp [split $x]
    switch -- [string tolower [lindex $sp 0]] {
	ping {
	    puts $sock "PONG [string trimleft [join [lrange $sp 1 end]] :]"
	}
    }
}

proc apc_log sock {
    fileevent $sock writable ""
    puts $sock "NICK [rindex [split abcdefghijklmnopqrstuvwxyz_`^\{\}\[\] \
	    {}]][ndig [format %X [rand 0 999999999]] 8]"
    #how many args go here? blah
    puts $sock "USER unf unf unf unf unf unf unf unf unf unf unf unf unf unf \
	    unf"
    global apcport
    set apcport [list [lindex [fconfigure $sock -sockname] 2] [lindex \
	    [fconfigure $sock -peername] 2]]
}

foreach i [channels] {
    set chantopic([string tolower $i]) [topic $i]
    catch {refresh [list $i]}
}
proc topic chan {
    global chantopic
    if {![info exists chantopic([string tolower $chan])]} {return} \
	    else {return $chantopic([string tolower $chan])}
}

proc istitle x {expr {[lsearch -exact [get_cookie title] $x] != -1}}

proc refresh window {
    global umode away isaway awm lserver connecting serverlag whoami env \
	    dccz lastlag
    if {$window == "status"} {
	if {[connected]} {
	    set um $umode
	} elseif {!$connecting} {
	    set um disconnected
	} else {
	    set um connecting
	}
	if {![info exists isaway]} {set isaway 0}
	if {$isaway} {
	    set aw [expr {[unixtime] - $away}]
	    set ad [expr {$aw / 3600}]
	    append ad :[string range [set ont 0[expr {($aw / 60) % 60}]] \
		    [expr {[string length $ont] - 2}] end]
	    if {[get_cookie seconds]} {append ad :[string range [set ont \
		    0[expr \
		    {$aw % 60}]] [expr {[string length $ont] - 2}] end]}
	    set awy " away($ad)"
	} else {
	    set awy ""
	}
	if {![istitle away]} {set awy ""}
	if {[connected]} {
	    if {[istitle uhost]} {
		set mz "[my_user]@[join [lrange [set s [split \
		    [my_host] .]] [expr {[llength $s] - 2}] end] .], "
	    } else {set mz ""}
	    set my ": [lindex $lserver 0]([lindex [server] 1])"
	    if {[istitle for]} {append my " for([get_cookie ot])"}
	    if {[istitle away]} {append my $awy}
	    if {[istitle lag]} {
		set lagz $serverlag
		if {$lagz != $lastlag && $lastlag != ""} {
		    set fresh 1
		} else {set fresh 0}
		if {$lastlag < 1 && $lastlag != ""} {
		    set lastlag [string trimleft [string range [format %1.3f \
			    $lastlag] 2 end] 0]
		    set lsec ms
		} else {set lsec s}
		if {$lagz < 1} {
		    set lagz [string trimleft [string range [format %1.3f \
			    $lagz] 2 end] 0]
		    set sec ms
		} else {set sec s}
		append my " lag($lagz$sec)"
		set lastlag $serverlag
	    }
	    if {[get_cookie cloak]} {append my " \[cloaked\]"}
	} else {
	    set mz ""
	    set my ""
	}
	if {[window get_title status] != [set newt "[my_nick]\[$mz$um\]$my \
		$dccz"]} {
	    window set_title $newt status
	}
    } elseif {[string match {[#&]*} $window]} {
	if {[connected]} {
	    set mode ""
	    if {[isvoice [my_nick] $window]} {set mode +}
	    if {[isop [my_nick] $window]} {set mode @}
	    global splitchan
	    set window [string tolower $window]
	    set min [list]
	    if {[info exists splitchan($window)]} {
		set l [llength $splitchan($window)]
		if {$l != 0} {
		    set min ", $l split"
		}
	    }
	    if {[window get_title channel $window] != [set newt \
		    "$window\[[llength [chanlist $window]]$min\]\
		    (+[mode $window]):[expr {[strep [get_cookie \
		    lock([string tolower $window])]] != "" ? "*" : ""}]\
		    [string trim [strip [topic $window]] " "]"]} {
		window set_title $newt channel $window
	    }
	} else {
	    if {[window get_title channel $window] != $window} {
		window set_title $window channel $window
	    }
	}
    } elseif {[queries $window] != "" && ![string match .*. $window] && \
	    ![string match *:\[0-9\]* $window]} {
	set awx ""
	if {[info exists awm($window)]} {
	    if {$awm($window) != ""} {
		set awx " away([strip $awm($window)])"
	    }
	}
	set chx ""
	foreach i [channels] {
	    if {[ison $window $i]} {lappend chx $i}
	}
	if {$chx == ""} {
	    set chx none
	} else {
	    set chx [join $chx ,]
	}
	set uh ([uhost $window])
	if {$uh == "()"} {set uh ""}
	if {[window get_title query $window] != [set newt "$window$uh\
		same\[$chx\]$awx"] && [connected]} {window set_title $newt \
		query $window} elseif {![connected] && [window get_title \
		query $window] != $window} {window set_title $window query \
		$window}
    }
}

proc do_timer time {
    global timer
    set id    [lindex $time 0]
    set do    [lindex $time 1]
    set times [lindex $time 2]
    set int   [lindex $time 3]
    set last  [lindex $time 4]
    if {[unixtime] - $last >= $int} {
	if {[catch {uplevel 1 $do} msg]} {
	    echo "[kano] Error in timer$id: $msg"
	}
	set last [unixtime]
    }
    if {$times > 1} {
	set timer($id) [list $id $do [expr {$times - 1}] $int $last]
    } elseif {$times == "0"} {
	set timer($id) [list $id $do $times $int $last]
    } else {
	unset timer($id)
    }
}

proc strip {str {type orubcg}} {
    set type [string tolower $type]
    if {[string first b $type] != -1} {regsub -all  $str "" str}
    if {[string first u $type] != -1} {regsub -all  $str "" str}
    if {[string match {*[rv]*} $type]} {regsub -all  $str "" str}
    if {[string first o $type] != -1} {regsub -all  $str "" str}

    if {[string first c $type] != -1} {
	regsub -all {(([0-9])?([0-9])?(,([0-9])?([0-9])?)?)?} $str "" str
    }

    if {[string first g $type] != -1} {
	regsub -all -nocase {([0-9A-F][0-9A-F])?} $str "" str
    }
    return $str
}

proc perm str {
    if {[string length $str] == 2} {return [list $str [string index $str 1] \
	    [string index $str 0]]}
    set FINAL [list]
    for {set x 0} {$x < [string length $str]} {incr x} {
	set first [string range $str 0 [expr {$x-1}]]
	set char [string index $str $x]
	set last [string range $str [expr {$x+1}] end]

	foreach i [perm $first$last] {
	    append FINAL "[string range $i 0 [expr {$x-1}]]$char[string \
		    range $i $x end] "
	}
    }
    return $FINAL
}

proc dccs {} {
    set dccs 0
    set dccx ""
    while {$dccs != [dcc_count]} {
	set dcci [dcc_info $dccs]
	lappend dccx $dcci
	incr dccs
    }
    return $dccx
}

proc ip2long ip {
    return [FALC_ip2long $ip]
}

proc long2ip ip {
    return [FALC_long2ip $ip]
}


proc rindex string {
    return [lindex $string [rand 0 [expr {[llength $string] -1}]]]
}

proc userlist {} {
    set userlist ""
    for {set i 0} {$i < [user_count]} {incr i} {
	lappend userlist [get_user $i mask]
    }
    return [string tolower $userlist]
}

proc trim_selection {} {
    set len [input get_sel_length]
    set txt [input get_text]
    set str [input get_sel_start]
    set txt0 [string range $txt 0 [expr {$str - 1}]]
    set txt1 [string range [string trim $txt] $str [expr {$str+$len-1}]]
    set txt2 [string range $txt [expr {$str+$len}] end]
    set lspaces [expr {[string length $txt1] - [string length [string \
	    trimleft $txt1]]}]
    set rspaces [expr {[string length $txt1] - [string length [string \
	    trimright $txt1]]}]
    set txt1 [string range [string trim $txt] [expr {$str+$lspaces}] [expr \
	    {$str+$len-1-$rspaces}]]
    set txt1 [format %-${lspaces}s ""]$txt1[format %-${rspaces}s ""]
    input set_text $txt0$txt1$txt2
    input set_sel_start [expr {$str+$lspaces}]
    input set_sel_length [expr {$len-$rspaces}]
}

proc format_in char {
    set len [input get_sel_length]
    if {$len == 0} return
    trim_selection
    set len [input get_sel_length]
    set txt [input get_text]
    set str [input get_sel_start]
    set start [string range $txt 0 [expr {$str -1}]]
    set middl [string range $txt $str [expr {$str+$len -1}]]
    set endst [string range $txt [expr {$str+$len}] end]
    if {[string index $middl 0] == $char} {
	set oldmid $middl
	set middl [string trim $middl $char]
	incr len -[expr {[string length $oldmid]-[string length $middl]}]
    } else {
	set middl $char$middl$char
	incr len 2
    }
    input set_text $start$middl$endst
    input set_sel_start $str
    input set_sel_length $len
    complete
}

proc final_in {} {
    if {![input get_sel_length]} return
    set text [input get_text]
    set in [input get_sel_start]
    set len [input get_sel_length]
    input set_text [string range $text 0 [expr {$in -1}]][string range $text \
	    [expr {$in+$len}] end]
    input set_sel_start $in
}

proc abcs {} {
    return abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890
}

proc ddirname file {return [file dirname $file]}

proc eq {a b} {expr {[string compare $a $b] == 0}}

rename say mysay
proc say text {
    set text [string trim $text \r\n]
    if {[string first \n $text] != -1} {
	foreach line [split $text \r\n] {
	    say $line
	}
	return
    }
    global curwin
    if {![get_cookie onewindow 0] || [string index [input get_text] 0] == \
	    "/" || [lindex $curwin(chan) 0] == "status"} {
	mysay $text
	return
    }
    switch -- [lindex $curwin(chan) 0] {
	channel {/msg [lindex $curwin(chan) 1] $text}
	query {/msg [lindex $curwin(chan) 1] $text}
	chat {/msg =[lindex $curwin(chan) 1] $text}
    }
    complete
}

foreach i {channel query chat} {
    rename $i my$i
    proc $i {} [subst {
	global curwin
	if {\[lindex \$curwin(chan) 0\] == "$i" && [get_cookie onewin 0]} \
		{return \[lindex \$curwin(chan) 1\]} elseif {![get_cookie \
		onewin 0]} {return \[my$i\]} {return ""}
    }]
}

#rename window mywindow
proc xwindow args {
    global curwin wintitle
    if {![get_cookie onewindow 0]} {
	if {[lindex $args 0] == "set_title"} {
	    if {[lindex $args 1] == [window get_title [lindex $args 2] \
		    [lindex $args 3]]} return
	}
	return [eval mywindow $args]
    }
    switch -- [lindex $args 0] {
	type {return [lindex $curwin(chan) 0]}
	name {return [lindex $curwin(chan) 1]}
	focus {/window [lindex $args 1] [lindex $args 2]}
	restore {}
	maximize {}
	set_title {
	    set wintitle([set title [string trim [string tolower "[lindex \
		    $args 2]-[lindex $args 3]"] -]]) [lindex $args 1]
	    if {[curwindow eq [lindex $args 2] [lindex $args 3]]} {
		mywindow set_title [lindex $args 1] status
	    } elseif {[lindex [string tolower $args] 2] == "status"} {
		mywindow set_title [concat $wintitle(main) [lindex [string \
			tolower $args] 1]] main
		set wintitle(status) [lindex $args 1]
	    } elseif {[lindex [string tolower $args] 2] == "main"} {
		set wintitle(main) [lindex $args 1]
		mywindow set_title [concat $wintitle(main) \
			$wintitle(status)] main
	    }
	}
	get_title {
	    if {![info exists wintitle([set title [string trim [string \
		    tolower "[lindex $args 1]-[lindex $args 2]"] -]])]} \
		    {
		return
	    } else {
		return $wintitle($title)
	    }
	}
	default {catch "mywindow $args" x;return $x}
    }
}

proc curwindow {{cmd ""} {type ""} {name ""}} {
    global curwin
    switch $cmd {
	"" {return $curwin(chan)}
	color {return $curwin(color)}
	eq {
	    if {$type == ""} {return 1}
	    set type [string tolower $type]
	    set name [string tolower $name]
	    if {$name == ""} {return [expr {$type == [lindex $curwin(chan) \
		    0]}]}
	    return [expr {[list $type $name] == $curwin(chan)}]
	}
    }
}

proc bright x {
    set m ""
    foreach i $x {
	switch -- [string trimleft $i 0] {
	    2 {lappend m 12}
	    3 {lappend m 09}
	    5 {lappend m 04}
	    6 {lappend m 13}
	    7 {lappend m 08}
	    10 {lappend m 11}
	    14 {lappend m 15}
	    15 {lappend m 00}
	    default {
		set i 0$i
		lappend m [string range $i [expr {[string length $i] - 2}] end]
	    }
	}
    }
    return $m
}

proc fzero num {
    scan $num %d num
}

proc converse list {
    global converse
    set l ""
    foreach i $list {
	scan $i %d i
	set f [lsearch -exact $converse $i]
	if {![expr {$f%2}]} {
	    #the index is odd, counting by ones
	    set i [lindex $converse [expr {$f + 1}]]
	} else {set i [lindex $converse [expr {$f -1}]]}
	lappend l $i
    }
    set temp [lindex $l 1]
    if {$temp != ""} {set temp [format %.2d [lindex $l 1] 2]}
    return [list [lindex $l 0] $temp]
}

proc mkfile string {
    regsub -all {[-/\\\"?|<>*]} $string _ string
    return $string
}

#logging!
proc autolog {string chan name} {
    if {![get_cookie autolog 0]} {kill_logs; return}
    if {[string match .*. $name] || ![window exists $chan $name]} return
    set chan [mkfile $chan]
    set name [mkfile $name]
    set lchan [string tolower $chan]
    set lname [string tolower $name]
    if {$lchan == "status" || $lname == ""} {set file [file join logs \
	    $lchan]} else {set file [file join logs $lchan $lname]}
    set file $file.txt
    global logf
    set new 0
    if {![info exists logf($file)]} {
	catch {file mkdir [file dirname $file]}
	if {[catch {open $file a} x]} {return}
	fconfigure $x -blocking 0 -buffering line
	set logf($file) $x
	if {[catch {puts $logf($file) "\[ started logging at [clock format \
		[clock seconds]] \]"}]} {
	    catch {close $logf($file)}
	    unset logf($file)
	}
    }
    if {[catch {puts $logf($file) \
	    "[clock format [clock seconds] -format \
	    [get_cookie autologtimefmt "\[%m/%d/%y %H:%M\]"]]\
	    [strip $string]"}]} {
	close $logf($file)
	unset logf($file)
	autolog $string $chan $name
    }
}

proc kill_logs {} {
    global logf
    foreach i [array names logf] {
	catch {puts $logf($i) "\[ stopped logging at [clock format [clock \
		seconds]] \]"}
	catch {close $logf($i)}
    }
    catch {unset logf}
}


proc analyze {string {no ""}} {
    set str ""
    foreach string [split $string \n] {
	for {set i 1} {$i <= 31} {incr i} {
	    if {$i == 10} continue
	    set f [format %c $i]
	    regsub -all $f $string \n[format %c [expr {$i + 64}]]\n string
	}
	if {$no == ""} {regsub -all \n $string  string}
	lappend str $string
    }
    return [join $str \n]
}

proc timeit {event times} {
    set z [clock clicks]
    for {set x 0} {$x < $times} {incr x} {uplevel #0 $event}
    return [expr {[clock clicks] - $z}]
}

proc ech {type {echo ""}} {
    set echo [string trim $echo \n]
    if {[string first \n $echo] != -1} {
	set final [list]
	foreach line [split $echo \n] {
	    lappend final [ech $type $line]
	}
	return [join $final \n]
    }

    global kanobg echoExp kcolorloaded
    if {![regexp $echoExp(color) $kanobg junk dfg junk dbg]} {set dfg \
	    15;set dbg 1}
    if {$kcolorloaded} {
	switch -- $type {
	    ini -
	    converse -
	    bold {
		set x $type
	    }
	    kill {
		set x fix
	    }
	    default {return $echo}
	}
	return [COLOR_$x $echo $dfg $dbg]
    }
    switch -- $type {
	split {
	    lappend end [list {} {}]
	    while {[regexp -indices $echoExp(color) $echo indices fg \
		    junk bg]} {
		set fg [eval string range [list $echo] $fg]
		set bg [eval string range [list $echo] $bg]
		if {$bg == "" && $fg == ""} {set bg $dbg;set fg $dfg}
		if {$fg != ""} {
		    scan $fg %d fg
		    set fg [expr {$fg%16}]
		}
		if {$bg != ""} {
		    scan $bg %d bg
		    set bg [expr {$bg%16}]
		}
		lappend end [string range $echo 0 [expr {[lindex $indices 0] \
			-1}]] [list $fg $bg]
		set echo [string range $echo [expr {[lindex $indices 1] +1}] \
			end]
	    }
	    lappend end $echo
	    set echo $end
	}
	bold {
	    set bold 0
	    set fg $dfg
	    set bg $dbg
	    set end [list]
	    foreach {color type} [ech split $echo] {
		foreach {a b} $color {}
		append end 
		if {$a != ""} {
		    set fg $a
		    set temp [expr {$bold ? [bright $fg] : $fg}]
		    scan $temp %d temp
		    append end [format %.2d $temp]
		}
		if {$b != ""} {
		    set bg $b
		    append end ,[format %.2d $bg]
		}
		while {[regexp -indices  $type b]} {
		    set bold [expr {!$bold}]
		    set temp [expr {$bold ? [bright $fg] : $fg}]
		    scan $temp %d temp
		    set type [string range $type 0 [expr {[lindex $b 0] \
			    -1}]][format %.2d $temp][string range $type \
			    [expr {[lindex $b 1] +1}] end]
		}
		append end $type
	    }
	    set echo $end
	}
	converse {
	    set end [list]
	    set fg $dfg
	    set bg $dbg
	    foreach {color type} [ech split $echo] {
		foreach {a b} $color {}
		if {$a != ""} {set fg $a}
		if {$b != ""} {set bg $b}
		if {$a == $b} {set fg $dfg;set bg $dbg}
		foreach {a b} [converse [list $fg $bg]] {}
		append end $a,[format %.2d $b]
		append end $type
	    }
	    set echo $end
	}
	kill {
	    set fg $dfg
	    set bg $dbg
	    set end [list]
	    foreach {color type} [ech split $echo] {
		foreach {a b} $color {}
		append end 
		if {$a != ""} {set fg $a}
		if {$b != ""} {set bg $b}
		if {$fg == $bg} {set fg [lindex [converse [list $fg 0]] \
			0];scan $fg %d fg}
		if {$a != ""} {append end [format %.2d $fg]}
		if {$b != ""} {append end ,[format %.2d $bg]}
		append end $type
	    }
	    set echo $end
	}
	ini {
	    set fg $dfg
	    set bg $dbg
	    set end [list]
	    foreach {color type} [ech split $echo] {
		foreach {a b} $color {}
		if {$a != ""} {set fg $a}
		if {$b != ""} {set bg $b}
		append end [c2g $fg $bg] $type
	    }
	    set echo $end
	}
    }
    return $echo
}

#..thanks conio!
proc ansitomirc string {
    global kcolorloaded echoExp kanobg
    if {$kcolorloaded} {
	if {![regexp $echoExp(color) $kanobg junk dfg junk dbg]} {set dfg \
		15;set dbg 1}
	return [COLOR_ansi $string $dfg $dbg]
    }
    set colors [list {01 14} {05 04} {03 09} \
	    {07 08} {02 12} {06 13} \
	    {10 11} {15 16}]
    set darks {01 05 03 07 02 06 10 15}
    set darks2 {01 02 03 05 06 07 10 15}
    set brights {14 12 09 04 13 08 11 00}

    set ansis {30 31 32 33 34 35 36 37 40 41 42 43 44 45 46 47}

    set bold 0
    set fgflag "15,"

    set newstring ""
    while {[regexp -indices {\[([0-9;]*)([mC])} $string \
	    full num char]} {

	append newstring [string range $string 0 [expr {[lindex $full 0] -1}]]
	set num [string range $string [lindex $num 0] [lindex $num 1]]
	set char [string range $string [lindex $char 0] [lindex $char 1]]

	set string [string range $string [expr {[lindex $full 1] + 1}] end]
	switch -- $char {
	    m {
		foreach q [lsort [split $num ";"]] {
		    set r [lsearch -exact $ansis $q]
		    if {$r != -1} {
			if {$r < 8} {
			    set fgflag [lindex [lindex $colors $r] $bold]
			    append newstring $fgflag
			    append fgflag ","
			} else {
			    append newstring $fgflag [lindex $darks [expr \
				    {$r-8}]]
			}  
		    } else {
			switch -- $q {
			    0 {
				set bold 0
				append newstring 
			    }
			    1 {
				set bold 1
				set d [lsearch -exact \
					$darks2 [string range $fgflag 1 2]]
				if {$d != -1} {
				    set fgflag "[lindex $brights $d],"
				}
			    }
			    4 -
			    7 {
				append newstring 
			    }
			}   
		    }   
		}   
	    }
	    C {
		if {![catch {format %*s $num ""} val]} {
		    set string $val
		}
	    }
	}
    }
    append newstring $string
    return $newstring
}

proc ctcp2 string {
    while {[set 1 [string first  $string]] != -1} {
	set 2 [string first  [string range $string [expr {$1 +1}] end]]
	if {$2 == -1} {set 2 $1} else {set 2 [expr {$2+$1+1}]}
	set tag [string tolower [string range $string [expr {$1 +1}] [expr \
		{$2 -1}]]]
	set new ""
	switch -glob -- $tag {
	    b- -
	    b {set new }
	    s- -
	    s  -
	    i- -
	    i  -
	    u- -
	    u {set new }
	    v- -
	    v {set new }
	    c* {
		global ansivc
		if {[string index $tag 1] == "#"} {set tag [string index \
			$tag 0][format %X [rgb2irc [string range $tag 1 \
			7]]][string range $tag 8 end]}
		if {[string index $tag 2] == "#"} {set tag [string range \
			$tag 0 1][format %X [rgb2irc [string range $tag 2 \
			end]]]}
		set fg [string index $tag 1]
		set bg [string index $tag 2]
		if {$fg == "." || $fg == ""} {
		    set fg ""
		} else {
		    catch {set fg $ansivc([format %i 0x$fg])}
		}
		set new $fg
		if {$bg != ""} {
		    catch {set bg $ansivc([format %i 0x$bg])}
		    append new ,$bg
		}
	    }
	}
	set string [string range $string 0 [expr {$1 -1}]]$new[string \
		range $string [expr {$2 +1}] end]
    }
    return $string
}

proc irc2rgb irc {
    global rgb2irc ansicv ansivc
    scan $irc %d irc
    if {[info exists rgb2irc($irc)]} {
	set i $rgb2irc($irc)
	return \#[format %02X%02x%02x [lindex $i 0] [lindex $i 1] \
		[lindex $i 2]]
    }
}

proc rgb2irc rgb {
    if {![regexp -nocase \
	    {^#?([a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9])$} $rgb \
	    junk rgb]} {return}
    global rgb2irc ansicv
    set r [format %i 0x[string range $rgb 0 1]]
    set g [format %i 0x[string range $rgb 2 3]]
    set b [format %i 0x[string range $rgb 4 5]]
    set close 765
    set this {}
    foreach i [lsort -integer [array names rgb2irc]] {
	foreach {nr ng nb} $rgb2irc($i) {}
	set diff [expr {abs($r - $nr) + abs($g - $ng) + abs($b - $nb)}]
	if {$diff < $close} {set close $diff;set this $i}
    }
    return $ansicv($this)
}

proc myecho args {eval base_echo $args}

set echoExp(sub) "( |^)(ftp|www)\\.(((\[a-z0-9-\]*)(\[\001-\038\]|$)).*)?$"
set echoExp(exp) "^.*(://|(www|ftp)\.)(\[^\001-\038\]*)((\n| ).*)?$"
set echoExp(exp2) {([!-.:-@\{-~[-`]*)$}
set echoExp(color) {([0-9]?[0-9]?)(,([0-9]?[0-9]?))?}

rename echo base_echo
proc myecho args {eval base_echo $args}
proc my_real_echo args {eval base_echo $args}
proc echo {string {channel ""} {name ""}} {
    global relaytext kanobg grep_for onEcho analyze end_urls redir \
	    redir_text isredir rdecho delredir echolines kcolorloaded echoExp
    #are we using onEcho to display a myjoin msg?

    set wintype [window_type]
    set winname [window_name]

    if {$kcolorloaded} {
	if {![regexp $echoExp(color) $kanobg junk dfg junk dbg]} {set dfg \
		15;set dbg 1}
	set string [COLOR_simple $string $dfg $dbg]
	set string [COLOR_ansi $string $dfg $dbg]
    } else {
	regsub -all \r $string {} string
	set string [string trim $string \n]
    }
    if {$string == ""} return
   
    if {$channel == ""} {set chon $wintype} else {set chon $channel}
    if {$name == ""} {set nom $winname} else {set nom $name}
    set nom [string tolower $nom]
    if {[info exists onEcho($nom)]} {
	if {[window exists $chon $nom]} {
	    if {[string index $onEcho($nom) 0] != "\014" && [info exists \
		    relaytext($nom)]} {
		foreach i [lrange $relaytext($nom) [expr {[llength \
			$relaytext($nom)] - $echolines}] end] {
		    base_echo $i $chon $nom
		}
	    }
	    set onecho $onEcho([string tolower $nom])
	    unset onEcho([string tolower $nom])
	    echo [string trimleft $onecho \014] $chon $nom
	}
    }
    #parse the text a bit:
    #- parse ansi
    #- parse tab characters
    #- make sure $kanobg is used after ctrl-o (original text)

    if {!$kcolorloaded} {
	set echo $kanobg$string
	#more kanobg stuff: empty ctrl-c tags (original color)
	regsub -all {([^0-9,])} $echo $kanobg\\1 echo
	
	if {[string first \t $echo] != -1} {
	    regsub -all \t $echo {        } echo
	}
	if {[string first  $echo] != -1} {
	    catch {set echo [ansitomirc $echo]}
	}
	if {[string first \n $echo] != -1} {
	    regsub -all \n $echo \n echo
	} 
	if {[string first  $echo] != -1} {
	    regsub -all  $echo $kanobg echo
	}
    } else {
	set echo $string
    }

    global echoExp

    while {[regexp -nocase -indices $echoExp(sub) $echo 0 1 2 3]} {
	set echo [string range $echo 0 [lindex $2 1]].[string \
		range $echo [lindex $3 0] end]
    }

    #what's this for? we're making it so url's can be clicked-on without
    # appending any trailing punctuation to the end of the actual url
    # example: "go to www.kano.net, it's really cool!"
    # despite the bad grammar in that sentence, the comma won't be
    # underlined by xircon and your browser won't try going to
    # "www.kano.net,"

    while {[regexp -nocase -indices $echoExp(exp) $echo junk junk \
	    junk indices]} {
	if {![regexp -indices $echoExp(exp2) [eval [list string range $echo] \
		$indices] end]} continue
	set 0 [expr {[lindex $indices 0] + [lindex $end 0]}]
	set echo [string range $echo 0 [expr {$0 -1}]][string range \
		$echo $0 end]
    }
    set orig $string
    set string $echo

    global addechos
    set STOP 0
    if {[info exists addechos]} {
	foreach i $addechos {
	    if {$STOP} return
	    catch $i
	}
    }
    #relaytext stuff (/relay, /paste)
    set el $echolines
    if {$el} {
	set t $wintype
	if {$channel == ""} {
	    set channel $t
	    set name $winname
	}
	if {$name == ""} {
	    set name $channel
	    if {$t != "status"} {
		set name [string tolower $winname]
	    } else {
		set name status
	    }
	}
	if {$channel == "status"} {set name status}
	set name [string tolower $name]
	#8.9ms
	lappend relaytext($name) $echo
    }
    base_echo $echo $channel $name
}

proc c2g {{color 0} {bg 1}} {
    scan $color %d color
    scan $bg %d bg
    if {[catch {expr {$color %16}} color] || [catch {expr {$bg %16}} bg]} \
	    {return $color,[ndig $bg 2]}
    global ansicv
    set fore [format %X $ansicv($color)]
    set back [format %X $ansicv($bg)]
    if {$bg != ""} {set bg [ndig $bg 2]}
    return $color,$bg$back$fore
}

proc ndig {str num} {
    if {[catch {scan $str %d str}]} {return $str}
    format %.*d $num $str
}

proc g2c {{color 0} {bg 1}} {
    if {$color == ""} {set color 0}
    if {$bg == ""} {set bg 1}
    set ansilist {0=1 1=2 2=3 3=10 4=5 5=6 6=7 7=15 8=14 9=12 10=9 11=11 \
	    12=4 13=13 14=8 15=0}
    set fg [split [lindex $ansilist [lsearch -glob $ansilist $color=*]] =]
    set bg [split [lindex $ansilist [lsearch -glob $ansilist $bg=*]] =]
    set fgc [lindex $fg 1]
    set fg [format %X [lindex $fg 0]]
    set bgc [lindex $bg 1]
    set bg [format %X [lindex $bg 0]]
    return $fgc,$bgc$bg$fg
}

proc punct {} {
    return {[:\(\)\?\.\,\!\;\"\!]}
}

proc punct2 {} {
    return :()\[\]\{\}.,/\\\;\"'-_=+`~!?
}

proc sortindex {index first second} {
    set x [lindex $first $index]
    set y [lindex $second $index]
    return [string compare $x $y]
}

proc writeconn {sck hp} {
    global issck snam
    fileevent $sck writable ""
    echo "[kano] Connection established." query $hp
    if {$snam($hp) != ""} {
	window set_title "$snam($hp) - Connected" query $hp
    } else {
	window set_title "$hp - Connected" query $hp
    }
}

proc getconn {sck hp} {
    global snam
    if {[eof $sck]} {
	catch {killconn $sck $hp}
	return
    }
    if {[lsearch [queries] $hp] == -1} {/query $hp;if {$snam($hp) != ""} \
	    {window set_title $snam($hp) query $hp}}
    echo "< [gets $sck]" query $hp
    if {[catch {flush $sck}]} {killconn $sck $hp}
}

proc killconn {sck hp} {
    global sock snam
    if {[info exists sock($hp)]} {
	unset sock($hp)
	echo [t closedconn] query $hp
	if {$snam($hp) != ""} {
	    window set_title "$snam($hp) - Connection closed" query $hp
	} else {
	    window set_title "$hp - Connection closed" query $hp
	}
	close $sck
    }
}

proc isn n {
    isnum $n
}


proc timer {time int scr {timerz ""}} {
    global timer 
    set m 1
    while {$timerz == ""} {
	if {[info exists timer($m)]} {incr m} else {set timerz $m}
    }
    set timer($timerz) "$timerz $time $time $int"
    lappend timer($timerz) $scr
    complete
}


proc round num {
    expr {round($num)}
}

proc country ctr {
    global countryl
    set ctr [string tolower $ctr]
    global countryl
    set f [lsearch -exact $countryl(a) $ctr]
    if {$f == -1} {return} else {return [lindex $countryl(b) $f]}
}

proc wordstart {string place} {
    set ret [string last " " [string range $string 0 [expr {$place-1}]]]
    if {$place > [string length $string]} {return $place} else {incr ret}
}

proc iph {bool ret ret2} {
    if {[uplevel 1 [list expr $bool]]} {
	return $ret
    } else {
	return $ret2
    }
}

proc nick2nick nick {
    set p ""
    set m ""
    foreach i [channels] {
	foreach j [chanlist $i] {
	    if {[lsearch $m $j] == -1} {lappend m $j}
	}
    }
    foreach i $m {
	foreach j [split [split $nick] ,] {
	    if {[string tolower $i] == [string tolower $j]} {
		if {[lsearch $p $i] == -1} {lappend p $i}
	    }
	}
    }
    if {$p == ""} {return $nick} else {return [join $p ,]}
}

proc nerr text {
    echo "[kano] $text" [get_cookie status]
}

proc err text {
    echo "[kano] $text" [get_cookie status]
}

proc listify list {
    if {[llength $list] == "1"} {
	return [join $list]
    } elseif {[llength $list] == "2"} {
	return "[lindex $list 0] and [lindex $list 1]"
    } elseif {$list == ""} {
	return ""
    } else {
	set g [lindex $list 0],
	foreach i [lrange $list 1 [expr {[llength $list] - 2}]] {
	    append g " $i,"
	}
	append g " and [lindex $list [expr {[llength $list] - 1}]]"
    }
}
	
proc add_tabnick nick {
    global tab_nix
    if {[string index $nick 0] != "/"} {
	if {[window exists query $nick]} {return}
	set nick "/msg $nick"
    }
    set new ""
    foreach i $tab_nix {
	if {[string tolower $i] == [string tolower $nick]} continue
	lappend new $i
    }
    set tab_nix "[list $nick] $new"
}

proc cform {{x ""}} {
    if {$x == ""} {return [clock format [clock seconds]]}
    return [clock format [clock seconds] -format $x]
}

proc clockf {{x ""} {y ""}} {
    if {$y == ""} {set y $x;set x [clock seconds]}
    return [clock format $x -format $y]
}

proc low x {string tolower $x}

proc addmlog text {
    global isaway kanobg
    if {!$isaway || ![get_cookie msglog]} return
    set x [open mlog.log a]
    set opq "$kanobg[cform "%a %I:%M"][low [string index [cform %p] 0]]-\
	    $text"
    set opq [rep [strip $opq g]  $kanobg]
    puts $x $opq
    close $x
    if {[window exists query .MsgLog.]} {echo $opq query .MsgLog.}
}

proc randvers {} {
    global vers
    set n {"Professor Plum" "Colonel Mustard" "Mr. Green" "Mrs. White" \
	    "Miss Scarlet" "Mrs. Peacock"}
    set b {"Ball Room" "Conservatory" "Billiard Room" "Library" "Kitchen" \
	    "Dining Room" "Hall" "Study" "Lounge"}
    set w {"Knife" "Candlestick" "Revolver" "Rope" "Lead Pipe" "Wrench"}
    set n [lindex $n [expr {[rand 1 [llength $n]] - 1}]]
    set b [lindex $b [expr {[rand 1 [llength $b]] - 1}]]
    set w [lindex $w [expr {[rand 1 [llength $w]] - 1}]]
    set x [lindex $vers [expr {[rand 1 [llength $vers]] - 1}]]
    regsub -all "&nick" $x [my_nick] x
    regsub -all "&n" $x $n x
    regsub -all "&b" $x $b x
    regsub -all "&w" $x $w x
    return $x
}

proc isprotect {nick type {state passive}} {
    if {[uhost $nick] != ""} {set nick [find_user $nick![uhost $nick]]} \
	    else {set nick [find_user $nick]}
    if {$nick == -1} {return 0}
    set notes [get_user $nick notes]
    if {[catch {llength $notes}]} {return 0}
    set f [lsearch $notes "protect *"]
    if {$f != "-1"} {
	set j [lindex $notes $f]
	if {[lindex $j 2] == $state || [lindex $j 2] == "active"} {
	    foreach i $type {
		if {[lsearch -exact [lindex $j 1] $type] != -1} {return 1}
	    }
	}
    }
    return 0
}

proc mask2real mask {
    return [real_user $mask]
}

proc real2mask real {
    return [is_user $real]
}

proc real_user mask {
    set mask [strep [string tolower $mask]]
    set masq ""
    foreach j [userlist] {
	if {[string match $mask $j]} {
	    return [find_user $j]
	    break
	}
    }
    return [find_user $mask]
}

proc is_user host {
    set masq [find_user $host]
    return $masq
}   

proc oknick nick {
    return ![regexp -nocase {[^]{}[a-z_`^\\|-]} $nick]
}

proc uhost nick {
    global who
    set l [string tolower $nick]
    if {[info exists who($l)]} {
	return $who($l)
    } else {return}
}

proc kv {} {return [get_cookie kver]}

proc kverz {} {return [get_cookie kverz]}

proc chanlist chan {
    set nix ""
    foreach i [nicks $chan] {
	lappend nix [string trim $i @%+]
    }
    return $nix
}

proc hostlist chan {
    set nix [list]
    foreach i [chanlist $chan] {
	lappend nix $i![uhost $i]
    }
    set nix
}

proc rand {start end} {
    if {$start >= $end} {return $start}
    return [random $start $end]
}

proc oplist chan {
    set nix ""
    foreach i [nicks $chan @*] {
	lappend nix [string trim $i @]
    }
    return $nix
}

proc noplist chan {
    set nix ""
    foreach i [nicks $chan] {
	if {![string match *@* $i]} {lappend nix [string trim $i @+]}
    }
    return $nix
}

proc vlist chan {
    set nix ""
    foreach i [nicks $chan +*] {
	lappend nix [string trimleft $i +]
    }
    return $nix
}

proc nvlist chan {
    set nix ""
    foreach i [nicks $chan] {
	set f [string index $i 0]
	if {[string first $f +@] == -1} {lappend nix [string trim $i +]}
    }
    return $nix
}

proc ison {nick chan} {
    regsub -all {\*|\?} $nick {\\&} nick
    llength [nicks $chan @$nick][nicks $chan +$nick][nicks $chan $nick]
}

proc isvoice {nick chan} {
    regsub -all {\*|\?} $nick {\\&} nick
    llength [nicks $chan +$nick]
}

proc isop {nick chan} {
    regsub -all {\*|\?} $nick {\\&} nick
    llength [nicks $chan @$nick]
}

proc arg {} {join [args]}

proc ctime unixtime {clock format $unixtime}

proc unixtime {} {clock seconds}

proc showj {{ag ""}} {
    global showj
    if {$ag == ""} {
	return $showj
    } else {
	set showj $ag
    }
}

proc uhash chan {
    global do_who
    if {$chan != ""} {
	incr do_who
	/quote WHO $chan
	set hashed([string tolower $chan]) 0
    }
}

proc getmon x {
    set days 1
    set mon [clock format $x -format %m]
    while {![catch {clock scan $mon/$days}]} {
	incr days
    }
    incr days -1
    return $days
}

proc since {time {precision 7}} {
    set now [clock seconds]

    if {[string index $time 0] == "c"} {
	set time [string range $time 1 end]
    } {
	if {[catch {expr {$now - $time}} time]} {
	    return ??
	}
    }
    set n [expr {$now < $time ? "-" : ""}]
    if {[catch {clock scan now -base $time}]} {return 0}

    set x(1year) 0
    set x(2month) 0
    set x(3week) 0
    set x(4day) 0
    set x(5hour) 0
    set x(6minute) 0
    set x(7second) 0

    array set map {
	year y
	month mn
	week w
	day d
	hour h
	minute m
	second s
    }

    set ex [expr {$time < $now}]
    set ago [expr {$ex ? "" : " ago"}]
    set expr [expr {$ex ? {$time < $now} : {$time > $now}}]
    set nexpr [expr {$ex ? {$next <= $now} : {$next >= $now}}]

    set last x
    while $expr {
	if {$time == $last} {
	    # infinite loop?
	    return 0
	}
	set last $time
	foreach unit [lsort [array names x]] {
	    set ru $unit
	    set unit [string range $unit 1 end]

	    set counter 10
	    while {[catch {clock scan "1 $unit$ago" -base $time} result] \
		    && $counter} {
		incr counter -1
	    }
	    if {$counter} {
		set next $result
	    } else {continue}
	    
	    if $nexpr {
		incr x($ru)
		set time $next
		break
	    }
	}
    }

    set out {}
    foreach name [lsort [array names x]] {
	set val $x($name)
	set num [string index $name 0]
	set name [string range $name 1 end]
	if {$val > 0} {
	    lappend out "$val$map($name)"
	}
	if {$num >= $precision && [llength $out]} {
	    break
	}
    }

    array set chmap {y year% mn month% w week% d day% h hour% m \
	    minute% s second%}

    set tmp [join $out " "]

    if {[get_cookie pron 0]} {
	set ntmp [list]
	foreach i $tmp {
	    if {![regexp {^([0-9]+)([a-z]+)$} $i junk str b] \
		|| ![info exists chmap($b)]} continue
	    set b $chmap($b)
	    if {$str == 1} {set s {}} else {set s s}
	    set str [pron_form $str]
	    regsub % $b $s b
	    lappend ntmp [concat $str $b]
	}
	if {[llength $ntmp] > 2} {
	    set tmp [concat [join [lrange $ntmp 0 [expr {[llength $ntmp] \
		    -2}]] ", "], and [lindex $ntmp end]]
	} else {set tmp [join $ntmp " and "]}
	if {$n == "-"} {set n negative}
	return [concat $n $tmp]
    } else {
	return $n[string trim $tmp]
    }
}

proc curwin {} {
    set x [window get_title status]
    if {![regexp {^\[([0-9]+)\] } $x junk win]} {set win 1}
    set win
}

proc these_wins {} {
    set x status
    foreach i [channels] {
	lappend x [list channel $i]
    }
    foreach i [queries] {
	lappend x [list query $i]
    }
    foreach i [chats] {
	lappend x [list chat $i]
    }
    set x
}

proc enum_wins {{win {}}} {
    if {$win == ""} {set win [curwin]}
    if {$win == [curwin]} {these_wins} else {
	global env
	set env(kanoenum) [list GET $win]
	while {[lindex $env(kanoenum) 0] != "SET" || [lindex \
		$env(kanoenum) 1] != $win} {
	    vwait env(kanoenum)
	}
	lindex $env(kanoenum) 2
    }
}

proc varproc {var array type} {
    global $var env whoami isaway quick_away away LOADING
    if {$LOADING} return
#    echo "[set var]([set array]) was [switch $type {w {format "written to \
#	[set ${var}($array)]"} u {format unset} r {format read}}]"
    switch -- $var {
	env {
	    switch -- $array {
		kanoloaded {
		    if {$env(kanoloaded) < $whoami} {
			set whoami $env(kanoloaded)
			if {$whoami == 1} {
			    echo "[kano] I am now the main kano window.\
				    Attempting to grab ident port." status
			    /getsocks
			} elseif {$whoami > 0} {
			    echo "[kano] I am now kano window $whoami." status
			}
		    }
		    if {$env(kanoloaded) > 1 && \
			    [llength [info commands kset_cookie]]} {
			save_cookies
			if {![catch {
			    proc save_cookies args return
			    rename get_cookie {}
			    rename set_cookie {}
			    rename kset_cookie set_cookie
			    rename kget_cookie get_cookie
			}]} {
			    echo "[kano] Turned config-caching off for this\
				    session due to multiple server windows."
			}
		    }
		}
		kanoaway {
		    if {[event] == "away"} return
		    if {!$isaway && [info exists env(kanoaway)]} {
			set quick_away 1
			set eq $env(kanoaway)
			/away [lindex $env(kanoaway) 1]
			set env(kanoaway) $eq
			set away [lindex $eq 0]
			refresh status
			set quick_away 0
		    }
		}
		kanoback {
		    if {$isaway} {
			/back $env(kanoback)
			set isaway 0
		    }
		}
		kanoenum {
		    echo $env(kanoenum)
		    if {[lindex $env(kanoenum) 0] == "GET" && [curwin] == \
			    [lindex $env(kanoenum) 1]} {
			set env(kanoenum) [list SET [curwin] [these_wins]]
		    }
		}
	    }
	}
    }
}

proc fix_rel_len {var name} {
    global $var
    set el [get_cookie echoLines 100]
    if {![info exists ${var}($name)]} {return}
    set r [set ${var}($name)]
    while {[set s [lsearch -glob $r *\n*]] != -1} {
	set l [lindex $r $s]
	set r [lreplace $r $s $s]
	foreach i [split $l \n] {
	    incr s
	    set r [linsert $r $s $i]
	}
    }

    set l [llength $r]
    if {$l > $el} {
	set ell [expr {$l-$el}]
	set r [lrange $r $ell end]
    }
    set ${var}($name) $r
}

proc nerre string {
    echo $string [get_cookie status]
}

proc add_link {server host hops} {
    global linkserv linkcur linknew oldlink
    if {$linknew} {
	catch {unset oldlink}
	foreach i [array names linkserv] {
	    set oldlink($i) $linkserv($i)
	}
	catch {unset linkserv}
	set linknew 0
    }
    lappend linkserv($host) $server
    if {$server == $host} {set linkcur $host}
    if {$hops == 0} {
	catch "lappend linkserv() $linkserv($host)"
	unset linkserv($host)
    }
}

proc link_tree {{var ""} {x "    "}} {
    global linkserv litehelp darkhelp linkcur
    if {$var == ""} {set var $linkserv()}
    set list ""
    set var [lsort $var]
    set dir ""
    set file ""
    foreach i $var {
	if {$i == $linkcur} continue
	if {[info exists linkserv($i)]} {lappend dir $i} else {lappend file $i}
    }
    set end [expr {[llength $file] -1}]
    set c 0
    set char0 Ã
    foreach i $file {
	if {$end == $c && $dir == ""} {set char0 À}
	append list "${darkhelp}$x$char0ÄÄÄ${litehelp}$i\n"
	incr c
    }
    if {[llength $dir] > 1} {set char1 ³} else {set char1 " "}
    set end [expr {[llength $dir] -1}]
    set c 0
    set char0 Ã
    foreach i $dir {
	if {$c == $end} {set char1 " ";set char0 À}
	append list "${darkhelp}$x$char0ÄÄÄ${litehelp}$i\n"
	if {[info exists linkserv($i)]} {
	    append list [link_tree $linkserv($i) "$x$char1   "]
	}
	incr c
    }
    return $list
}

proc show_links {} {
    global litehelp linkcur
    echo "${litehelp}$linkcur"
    echo [link_tree]
}

proc catch_links {} {
    global linknew oldlink linkserv
    set linknew 1
    if {[array exists oldlink]} {
	foreach i [array names linkserv] {
	    if {![info exists oldlink($i)]} {lappend net(left) [list $i \
		    $linkserv($i)]} else {
		foreach j $linkserv($i) {
		    if {[lsearch -exact $oldlink($i) $j] == -1} {lappend \
			    serv(left) $j}
		}
	    }
	}
	foreach i [array names oldlink] {
	    if {![info exists linkserv($i)]} {lappend net(joined) [list $i \
		    $oldlink($i)]} else {
		foreach j $oldlink($i) {
		    if {[lsearch -exact $linkserv($i) $j] == -1} {lappend \
			    serv(joined) $j}
		}
	    }
	}
	foreach i [array names net] {
	    echo "[kano] node:  $i: [listify $net($i)]"
	}
	foreach i [array names serv] {
	    echo "[kano] server:  $i: [listify $serv($i)]"
	}
    }
}

proc erre str {
    echo $str [get_cookie status]
}

proc fullvers {} {
    global verreply origver
    if {[get_cookie showadd 1]} {
	set ret $verreply
    } else {
	set ret $origver
    }
    set r [rindex [get_cookie mesg(clientinfo)]]
    if {$r != ""} {set r " :$r"}
    return $ret$r
}

proc setm {chan mode} {
    global SETM
    set x [list $chan $mode]
    if {![info exists SETM($x)]} {
	set SETM($x) [clock seconds]
    } else {
	if {[clock seconds] - $SETM($x) > 1} {
	    unset SETM($x)
	} else {
	    return 0
	}
    }
    return 1
}

proc inputcase type {
    set st [input get_sel_start]
    set sl [input get_sel_length]
    set tx [input get_text]
    if {$sl == 0} {
	input set_text [string to$type $tx]
    } else {
	input set_text [string range $tx 0 [expr {$st -1}]][string to$type \
		[string range $tx $st [expr {$st + $sl -1}]]][string range \
		$tx [expr {$st + $sl}] end]
    }
    input set_sel_start $st
    input set_sel_length $sl
    complete
}    

proc last_speak {chan a b} {
    global last_speak
    set chan [string tolower $chan]
    if {![info exists last_speak($chan)]} {return 0}
    set a [lsearch -exact $last_speak($chan) $a]
    set b [lsearch -exact $last_speak($chan) $b]
    if {$a == $b} {return 0} \
	    elseif {$a == -1} {return -1} \
	    elseif {$b == -1} {return 1} \
	    elseif {$a < $b} {return 1} \
	    elseif {$a > $b} {return -1}
}

proc lrem {string j} {
    set end ""
    foreach i $string {
	if {$i == $j} continue
	lappend end $i
    }
    return $end
}

proc instr {list item} {
    expr {[lsearch -exact $list $item] != -1}
}

proc build_alias {} {
    global help litehelp darkhelp
    set llength [llength [array names help]]
    
    set aliasz ""
    set aliasm ""
    set aliasn ""
    set mz 0
    set helpstuff ""
    set ok 0
    set colzr [list ${darkhelp}³ ${darkhelp}³]
    foreach i [array names help] {
	if {[string index $i 0] == "+" || [string index $help($i) 0] == \
		"!" || $i == "aliases"} continue
	incr ok
	lappend helpme([string tolower [file tail [lindex $help($i) 6]]]) $i
    }
    append aliasn "${darkhelp}ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿\n"
    append aliasn "${darkhelp}³${litehelp} [format %-3s $ok] aliases\
	    ${darkhelp}       ³\n"
    append aliasn \
	    "${darkhelp}ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿\n"
    foreach j [lsort [array names helpme]] {
	set z $helpme($j)
	if {$j != "" && [join $z] != ""} {
	    set betweena {ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿                            }
	    set betweenb {ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
	    append aliasn "${darkhelp}${betweena}³\n"
	    append aliasn "${darkhelp}³${litehelp} [format %-18s \
		    $j/[llength $z]] ${darkhelp}  ³[format %28s {}]³\n"
	    append aliasn "${darkhelp}${betweenb}´\n"
	}
	foreach i [lsort $z] {
	    if {[string index $i 0] == "+" || [string index $i 0] == "!"} \
		    continue
	    lappend helpstuff $i
	    if {[string length $i] <= 5} {
		append aliasm "[format %-5s [string tolower $i]] "
	    } elseif {[llength $aliasm] == [expr {7 - $mz}]} {
		set mz 1
		lappend aliasz $aliasm
		set aliasm "[format %-11s [string tolower $i]] "
	    } else {
		append aliasm "[format %-11s [string tolower $i]] "
		incr mz
	    }
	    if {[llength $aliasm] >= [expr {8 - $mz}]} {
		set mz 0
		lappend aliasz $aliasm
		set aliasm ""
	    }
	}
	if {$aliasm != ""} {lappend aliasz $aliasm}
	set which 0
	foreach i [lsort $aliasz] {
	    append aliasn "[lindex $colzr $which] ${litehelp}[format %-47s \
		    [string trim $i]] [lindex $colzr $which]\n"
	    set which [expr {!$which}]
	}
	set aliasz ""
	set mz 0
	set aliasm ""
    }
    set betweenc ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    append aliasn "${darkhelp}$betweenc\n"
    
    append aliasn "${litehelp}Get specific help: /help <alias>"
    set help(aliases) !$aliasn
}

proc isperm nick {
    if {[string match {[0-9]*} $nick]} {set x $nick} else {
	set x [find_user $nick]
    }
    if {$x == -1} {return -1}

    set notes [get_user $x notes]

    set f [lsearch -glob $notes "perm *"]
    if {$f == -1} {
	return -1
    } else {
	return [lindex [lindex $notes $f] 1]
    }
}

proc findlong list {
    set len 0
    foreach i $list {
	if {[string length $i] > $len} {set len [string length $i]}
    }
    return $len
}

proc repstr {str ct} {
    set end [list]
    for {set x 0} {$x < $ct} {incr x} {append end $str}
    return $end
}

proc roman x {
    set num 0
    set real [list 1000 500 100 50 10 5 1]
    set roman [list M D C L X V I]
    foreach char [lsort -decreasing -integer $real] {
	set val($num) [expr {$x / $char}]
	set x [expr {$x - ($val($num) * $char)}]
	incr num
    }

    set end [list]
    foreach i [lsort -integer [array names val]] {
	set f $val($i)
	if {!$f} continue
	set char [lindex $roman $i]
	set next [lindex $roman [expr {$i - 1}]]
	set next2 [lindex $roman [expr {$i -2}]]
	if {$f < 4 || $next == ""} {
	    append end [repstr $char $f]
	} elseif {$f < 6} {
	    append end [repstr $char [expr {5 - $f}]]$next
	} else {
	    append end [repstr $next [expr {10 - $f}]]$next2
	}
    }
    set ls M
    foreach {first second} [lrange $roman 1 end] {
	regsub -all $first$second$first $end $second$ls end
	set ls $second
    }
    return $end
}

set morsecode(morse) [list ._ _... _._. _.. . .._. __. .... .. .___ _._ ._.. \
	__ _. ___ .__. __._ ._. ... _ .._ ..._ .__ _.._ _.__ __.. .____ \
	..___ ...__ ...._ ..... _.... __... ___.. ____. _____]
set morsecode(text) [list a b c d e f g h i j k l m n o p q r s t u v w x \
	y z 1 2 3 4 5 6 7 8 9 0]

proc tomorse str {
    global morsecode
    regsub -all {[^a-z0-9]} [string tolower $str] {} str
    set end [list]
    for {set x 0} {$x < [string length $str]} {incr x} {
	lappend end [lindex $morsecode(morse) [lsearch -exact \
		$morsecode(text) [string index $str $x]]]
    }
    join $end
}

proc frommorse text {
    global morsecode
    set split [join [split [join $morsecode(morse) |] .] {\.}]
    set ev {regexp -indices -- $split $text indices}
    set thisev [eval $ev]
    if {!$thisev} {return $text}
    set end [list]
    set last 0
    while {$thisev} {
	set 0 [lindex $indices 0]
	set 1 [lindex $indices 1]
	append end [string range $text 0 [expr {$0-1}]]
	set this [string range $text $0 $1]
	append end [lindex $morsecode(text) [lsearch -exact \
		$morsecode(morse) $this]]
	set text [string range $text [expr {$1+1}] end]
	set thisev [eval $ev]
	if {!$thisev} {append end $text}
    }
    join $end {}
}

proc get_num num {
    foreach {a b} {0 {} 1 one 2 two 3 three 4 four 5 five 6 six 7 seven \
	    8 eight 9 nine 10 ten 11 eleven 12 twelve 13 thirteen 14 \
	    fourteen 15 fifteen 16 sixteen 17 seventeen 18 eighteen 19 \
	    nineteen 20 twenty 30 thirty 40 forty 50 fifty 60 sixty 70 \
	    seventy 80 eighty 90 ninety} {if {$num == $a} {return $b}}
    return $num
}

proc pron_form num {
    global pronounce
    set x [join [split $num ,] {}]
    set x [revorder [split $x {}]]
    set pron ""
    set ct [expr {[llength $pronounce] - 1}]
    foreach {a b c} $x {
	set p [pron_num $c$b$a]
	if {$p != ""} {
	    lappend pron "$p [lindex $pronounce $ct]"
	}
	incr ct -1
    }
    return [join [revorder $pron] ", "]
}

proc pron_num num {
    set num [string trimleft $num 0-]
    set hundred ""
    set ten ""
    set len [string length $num]
    if {$len == 3} {
	set hundred "[get_num [string index $num 0]] hundred"
	set num [string range $num 1 end]
    }
    if {$num > 20 && $num != $num/10} {
	set tens [get_num [string index $num 0]0]
	set ones [get_num [string index $num 1]]
	set ten [join [concat $tens $ones] -]
    } else {set ten [get_num $num]}
    return [concat $hundred $ten]
}


proc show_shell {name file} {
    if {[eof $file]} {close $file;return}
    echo [gets $file]
    update
}


proc findend {string init char} {
    set len [string length $string]
    for {set i [expr {$len -1}]} {$i <= 0} {incr i -1} {
	if {[string index $string $i] != " "} break
    }
    return [string range $string 0 $i]$init[rep [format %*s [expr \
	    {$len-1-$i}] ""] " " $char]
}

proc graph {color bg min max len {cutoff 0} {titlec1 01} {titlec2 01} \
	{title ""}} {
    if {$max > 0} {
	set percent [expr {round(100*(double($min)/$max))}]%
    } else {set percent 0%}
    set pc " ($percent)"
    set title [string range $title 0 [expr {$len -1-[string length $pc]}]]
    if {$title == ""} {
	set div [expr {$len/2}]
	set title [rep [format %${div}s $percent][format %*s [expr \
		{$len-$div}] ""] " " .]
    } else {
	append title $pc
    }

    if {$max > 0} {
	set fillsize [expr {round($len*(double($min)/$max))}]
	set emptysize [expr {$len-(round($len*(double($min)/$max)))}]
    } else {
	set fillsize 0
	set emptysize $len
    }

    if {$cutoff > 0} {
	if {$fillsize > $cutoff} {
	    set fillsize $cutoff
	    set emptysize 0
	} else {set emptysize [expr {$cutoff-$fillsize}]}
	set title [string range $title 0 [expr {$cutoff -1}]]
    }	

    set filled [findend $titlec1,$color[format %-${fillsize}s [string \
	    range $title 0 [expr {$fillsize -1}]]] $color,$color -]
    set empty [findend $titlec2,$bg[format %-${emptysize}s [string range \
	    $title $fillsize end]] " " $bg,$bg-]

    append ret $filled
    append ret $empty
    return $ret
}

proc happend {a b {margin 1}} {
    if {[llength $a] > [llength $b]} {set l [llength $a]} \
	    else {set l [llength $b]}
    set end ""
    set len 0
    foreach i $a {
	set lena [string length [strip $i]]
	if {$lena > $len} {set len $lena}
    }
    for {set i 0} {$i < $l} {incr i} {
	set 0 [lindex $a $i]
	set 1 [lindex $b $i]
	set dif [expr {[string length $0] - [string length [strip $0]]}]
	append end [format %-*s%-*s%s [expr {$len + $dif}] $0 ${margin} "" \
		$1]\n
    }
    return $end
}

proc filesize {size {no ""} {form %s}} {
    if {[catch {
	if {[catch {expr {$size / 1}}]} {
	    if {[catch {file size $size} x]} {return $size} else {set size $x}
	}
	if {$size < 1000} {
	    lappend end ${size} b
	} elseif {$size < 1000000} {
	    lappend end [expr {$size/1024.}] kb
	} elseif {$size < 1000000000} {
	    lappend end [expr {$size/1048576.}] mb
	} else {
	    lappend end [expr {$size/1073741824.}] gb
	}
	if {$no == ""} {
	    set end [expr {round([lindex $end 0])}][lindex $end 1]
	} else {
	    set end "[format $form [lindex $end 0]] [lindex $end 1]"
	}
    }]} {return $size} else {return $end}
}


proc masskick {chan nicks {msg {}}} {
    set ct 0
    if {$msg == ""} {
	set msg [rindex [get_cookie mesg(kick) {{i don't like you}}]]
    }
    global nonRFC
    if {$nonRFC} {
	foreach i $nicks {
	    /kick $chan $i $msg
	    incr ct
	}  
    } else {
	foreach {a b c d} $nicks {
	    /kick $chan [string trim [join [list $a $b $c $d] ,] ,] $msg
	    incr ct
	}
    }
    return $ct
}

proc maskhost host1 {
    if {![string match *!* $host1]} {set host1 *!$host1}
    if {![regexp {([^!]*)\!(~)?([^@]*)@(.*)} $host1 junk nick tilde user \
	    host]} {return $host1}
    set fullhost $host
    if {[string length $user] > 9} {
	set user [string range $user 0 7]*
    }
    if {[FALC_isip $host]} {
	if {![regexp {^([0-9]+\.[0-9]+\.[0-9]+)\.([0-9]+)$} $host junk main \
		small]} {return $host1} else {
	    set submask $main.*
	    set sub $main.$small
	    set host [list]
	}
    } else {
	if {![regexp {[^.]\.(.*)} $host junk end]} {set end [list]}
	if {![regexp {(.*)(\..+\..+)} $host junk sub host]} {set sub [list]} \
		else {set host [string trimleft $host .]}
	set submask [expr {$sub == "" ? "" : "*."}]
    }
    if {[catch {subst [get_cookie bantype {*!*$user@$submask$host}]} ret]} \
	    {set ret *!*$user@$submask$host}
    return $ret
}

proc lmatch {list match {nocase 1}} {
    set end [list]
    if {$nocase} {
	set list [string tolower $list]
	set match [string tolower $match]
    }
    while {[set f [lsearch $list $match]] != -1} {
	lappend end [lindex $list $f]
	set list [lrange $list [expr {$f+1}] end]
    }
    set end
}

proc compmode {chan mode arg} {
    set end ""
    set c [hostlist $chan]
    set lmy [string tolower [my_nick]]
    set list [chanlist $chan]
    set pm [string index $mode 0]
    set mode [string index $mode 1]
    if {[string first $mode be] == -1} {
	foreach i $arg {
	    if {$i != ""} {
		if {![ison $i $chan]} {
		    set strep [strep $i]
		    set m [lmatch $c $strep]
		    foreach j $m {
			regexp ^(.*)! $j junk nick
			if {[string tolower $nick] != $lmy} {
			    lappend end $nick
			} else {lappend end $i}
		    }
		} else {lappend end $i}
	    }
	}
    } else {set end $arg}
    set arg [lsort -command sort_slen $end]

    set final [list]
    set packet {}
    set modenum [get_cookie modenum 4]
    while {[llength $arg]} {
	set mod ""
	set x [llength $arg]
	if {$x > $modenum} {set x $modenum}
	while {$x > 0} {
	    append mod $mode
	    incr x -1
	}
	set nicks [lrange $arg 0 [expr {$modenum-1}]]

	set temp "\nMODE $chan $pm$mod [lindex $nicks 0]"

	if {[string length $temp] + [string length $packet] >= 512} {
	    lappend final $packet
	    set packet {}
	}

	append packet $temp
	set got 1
	foreach nick [lrange $nicks 1 end] {
	    set append " $nick"
	    if {[string length $packet$append] >= 512} {
		lappend final $packet
		set packet {}
		break
	    }
	    append packet $append
	    incr got
	}
	set arg [lrange $arg $got end]
    }
    if {$packet != ""} {
	lappend final $packet
    }
    set final
}

proc massmode {chan mode arg} {
    set m [compmode $chan $mode $arg]
    foreach i $m {
#	echo "\n----$i\n--([string length $i])--"
	/quote $i
    }
    return [llength $m]
}    

proc ret x {return $x}

proc killdupe list {
    set end ""
    foreach i $list {if {[lsearch -exact $end $i] == -1} {lappend end $i}}
    return $end
}

proc ajoin {channel {value ""}} {
    set c [split [get_cookie ajoin([curnet])] ,]
    set l [lsearch $c $channel]
    if {$value == ""} {
	set value [expr {$l != -1}]
    }
    if {$value} {
	if {$l == -1} {
	    lappend c $channel
	    echo "[kano] Added ajoin $channel."
	}
    } else {
	set nc [lreplace $c $l $l]
	if {$c != $nc} {
	    echo "[kano] Removed ajoin $channel."
	}
	set c $nc
    }
    set_cookie ajoin([curnet]) [join $c ,]
}

proc enrot {arg {key ""}} {
    if {$arg == ""} return
    if {$key == ""} {
	set key [get_cookie crypt_key x]
    }
    return [FALC_encrypt $key $arg]
}

proc rot {arg {key ""}} {
    if {[string index [lindex [args] 1] 0] == "§"} {return [rot_old $arg]}
    set arg [string range $arg 1 end]
    if {$arg == ""} return
    if {$key == ""} {
	set key [get_cookie crypt_key x]
    }
    return [FALC_decrypt $key $arg]
}

proc rot_old args {
    set n 0
    set text ""
    for {set i [expr {[string length [join $args]] - 1}]} {$i >= 0} \
	    {incr i -1} {
	set 1 [string index [join $args] $n]	
	scan $1 %c x
	set x [expr {$x + 128}]
	if {$x > 256} {set x [expr {$x - 256}]}
	set 1 [format %c $x]
	incr n
	append text $1
    }
    return $text
}

proc onlinet {} {
    global isaway away env newday
    if {![connected]} {set_cookie onlinet [expr {[get_cookie onlinet] + 1}]}
    set lastlog [get_cookie lastlog]
    set onlined [get_cookie onlined]

    set newday [expr {[clockf [get_cookie lastlog 0] %Y%m%d] \
	    < [clockf [clock seconds] %Y%m%d]}]
    set_cookie lastlog [clock seconds]
    if {[active]} {
	if {$newday} {
	    set_cookie onlined 0
	    set newday 0
	}
	if {[connected]} {
	    set_cookie onlined [expr {[get_cookie onlined] + 1}]
	}
    }

    if {[istitle for]} {
	set onlinet [expr {[clock seconds] - [get_cookie onlinet]}]
	set ot [expr {$onlinet / 3600}]
	append ot :[format %02d [expr {($onlinet / 60) % 60}]]
	if {[get_cookie seconds]} {append ot :[format %02d [expr \
		{$onlinet % 60}]]}
	set_cookie ot $ot
    }

    if {[istitle today]} {
	set od [expr {$onlined / 3600}]
	append od :[format %02d [expr {($onlined / 60) % 60}]]
	if {[get_cookie seconds]} {append od :[format %02d [expr \
		{$onlined % 60}]]}
    }

    if {[istitle client]} {
	set onlinec [expr {[clock seconds] - $env(cuptime)}]
	set oc ""
	if {$onlinec / 604800 != 0} {append oc [expr {$onlinec / 604800}]w}
	if {($onlinec / 86400) % 7 != 0} {append oc [expr {$onlinec / \
		86400 % 7}]d}
	append oc " [expr {($onlinec / 3600) % 24}]"
	append oc :[format %02d [expr {($onlinec / 60) % 60}]]
	if {[get_cookie seconds]} {append oc :[format %02d [expr \
		{$onlinec % 60}]]}
	set oc [string trim $oc]
    }

    if {[istitle os]} {
	set onlineo [osuptime]
	set oo ""
	if {$onlineo / 604800 != 0} {append oo [expr {$onlineo / \
		604800}]w}
	if {($onlineo / 86400) % 7 != 0} {append oo " [expr {($onlineo / \
		86400) % 7}]d"}
	append oo " [expr {($onlineo / 3600) % 24}]"
	append oo :[format %02d [expr {($onlineo / 60) % 60}]]
	if {[get_cookie seconds]} {append oo :[format %02d [expr \
		{$onlineo % 60}]]}
	set oo [string trim $oo]
    }

    if {$isaway && ![connected]} {incr away}

    set x "[get_cookie titlebar]\[[join [kanovers] {}]/[kclient xircon]\]"
    if {[istitle today]} {append x " today($od)"}
    if {[istitle client]} {append x " client($oc)"}
    if {[istitle os]} {append x " os($oo)"}
    if {[istitle time]} {
	append x " current([string trimleft [string tolower [clock format \
	    [unixtime] -format %I:%M%p]] 0])"
    }

    if {[window get_title main] != $x && [active]} {
	window set_title $x main
    }
}

proc createsock {x y z} {
    global apc
    after idle [list dnslookup $y]
    fileevent $x readable [list sendident $x $y]
    fconfigure $x -buffering line -blocking 0
    if {[info exists apc]} {return}
    if {[get_cookie show_ident 1]} {
	echo "[kano] $y connected to ident server.." status
    }
}

proc sendident {x y} {
    if {[cachedlookup $y]} {
	set y [dnslookup $y]
    }
    if {[catch {gets $x gets}]} {close $x; return}
    global clone_user klones apcport ident
    set id [get_cookie ident(user)]
    if {$id == 0 || $id == ""} {
	if {[catch {falc_config connection user} tempid] != 1} {
	    set id $tempid
	    set_cookie ident(user) $id
	} {
	    set id user
	}
    }
    set on [get_cookie ident(on)]
    set kloned 0
    if {[info exists klones]} {
	if {$klones != ""} {set id $clone_user[rand 1 [llength \
		$klones]];set on 1;set kloned 1}
    }
    set gets [split $gets]
    set lp [lindex $gets 0]
    set rp [lindex $gets 2]
    set isapc [expr {[list $lp $rp] == $apcport}]
    if {$isapc} {set on 1;set id [ndig [format %X [rand 0 999999999]] 10]}
    if {$on} {
	set gets [split $gets]
	if {![catch {puts $x "[join [lrange $gets 0 2]] : USERID : UNIX\
		: $id"}]} { 
	    if {!$kloned && !$isapc && [get_cookie show_ident 1]} {
		nerre [rep [t ident] %host $y]
	    }
	}
    } else {
	if {[get_cookie show_ident 1]} {
	    nerre [rep [t identoff] %host $y]
	}
    }
    catch {close $x}
}

proc about {} {
    global litehelp darkhelp
    set c1 ${darkhelp}
    set c2 ${darkhelp}
    set dash Ä
    set bar ³
    set n ¿
    set s À
    set e Ù
    set w Ú
    set seq $c2$dash$dash$c1$dash$dash
echo [string trim "\
$c1$w$seq$seq$seq$seq$seq$seq$seq$seq$seq$seq$seq$seq$c2$dash$n\n
$c2$bar15          You've loaded ${litehelp}kano.tcl [format %-7s [lindex\
	[kanovers] 1]15.]            $c1$bar\n
$c1$bar15      Visit [format %-37s [get_cookie MyKanoUrl \
	www.kano.net]]$c2$bar\n
$c1$s$dash$seq$seq$seq$seq$seq$seq$seq$seq$seq$seq$seq$seq$e\n
"] status
}

proc marq {text pos} {
    global marqing
    if {$marqing} {
	set in [input get_sel_start]
	set il [input get_sel_length]
	input set_text [string range $text $pos end][string range $text 0 \
		[expr {$pos-1}]]
	input set_sel_start $in
	input set_sel_length $il
	after 1000 {marq [input get_text] 1}
    }
}

proc wrap {str cols} {
    set m ""
    foreach str [split $str \n] {
	while {$str != ""} {
	    set j [wordstart $str $cols]
	    set add ""
	    if {$j == 0} {
		set add [string range $str 0 [expr {$cols -1}]]\n
		set str [string range $str $cols end]
	    } else {
		set add [string range $str 0 [expr {$j -1}]]\n
		set str [string range $str $j end]
	    }
	    if {$add != ""} {append m $add}
	}
	append m \n
    }
    return [string trim $m \n]
}

proc t type {
    set start [clock clicks]
    uplevel #0 [list set type $type]
    set up {subst [lindex $curtheme($type) 0]}
    switch -- $type {
	xdcclist {
	    set up {subst [lindex [rep [rep [rep [rep [rep [rep \
		    $curtheme($type) %file [string range [file tail \
		    [lindex $i 0]] 0 11]] %size [filesize [lindex $i 0]]] \
		    %gets [lindex $i 2]] %desc [lindex $i 1]] %num \$count] \
		    %realfile [lindex $i 0]] 0]}
	}
	who {
	    set up {subst [lindex [rep $curtheme($type) %blah [list $0 $4 \
		    $1 $2 $5 $6 $7]] 0]}
	}
	ujoin {
	    if {![uplevel \#0 [list info exists curtheme($type)]] || \
		    ![get_cookie ujoin 1]} {
		return [t join]
	    }
	}
    }
    if {[catch {uplevel \#0 $up} x] == 1} {
	global thmErr errorInfo
	set thmErr $x\n$errorInfo
	set x "THEME ERROR: $x"
    }
    return $x
}

proc getsocks {} {
    global identerr
    if {[catch {socket -server createsock 113} identerr]} {
	nerre [rep [rep [t socketfailed] %type Ident] %error $identerr]
    }
}

proc km_put {cmd arg} {
    return "$cmd [get_cookie mykanoserial] $arg"
}

proc s {num {def s} {def2 ""}} {expr {$num == 1 ? $def2 : $def}}

proc mssec ms {
    if {$ms < 0} {return 0ms}
    if {$ms < 1000} {return ${ms}ms} else {return [expr {$ms/1000.}]secs}
}

rename menu mymenu
proc menu args {
    if {[string index [join $args] 0] == "."} {
	eval "mymenu $args"
    } else {
	foreach {type path cmd} $args {}
	set type [lindex $args 0]
	set path [lindex $args 1]
	set cmd [lindex $args 2]
	mymenu $type [string tolower $path] $cmd
	global menuitems
	lappend menuitems($type) $path
    }
}

proc local {file {from {}}} {
    if {$from == ""} {set from [pwd]}
    set file [fixdots $file]
    set to [file split [file join [pwd] [string tolower [file dirname $file]]]]
    set from [file split [string tolower $from]]
    set len [llength $to]
    for {set inc 0} {$inc < $len} {incr inc} {
	if {[lindex $from $inc] != [lindex $to $inc]} break
    }
    set totaldots [lrange $from $inc end]
    set total [lrange $to $inc end]
    set dots [list]
    set len [llength $totaldots]
    for {set inc 0} {$inc < $len} {incr inc} {lappend dots ..}
    eval file join $dots $total [list [file tail $file]]
}

proc fixdots file {
    set end ""
    foreach i [file split $file] {
	set j [split $i {}]
	set islocal 1
	foreach k $j {
	    if {$k != "."} {set islocal 0;break}
	}
	if {$islocal} {
	    set end [lrange $end 0 [expr {[llength $end] -[string length $i]}]]
	} else {lappend end $i}
    }
    if {![catch [concat file join $end] end]} {
	return $end
    }
}

proc .. {file max} {
    set l [string length $file]
    if {$l > $max} {
	set dir [file dirname $file]
	set dir [expr {$dir == "." ? "" : $dir}]
	set file [file tail $file]
	set to [expr {$max - [string length $file] - 5}]
	if {$to < 0} {
	    set ext [file extension $file]
	    set name [file root $file]
	    set val [expr {[string length $name] -4 - abs($to)}]
	    if {$val < 0} {
		set next [expr {$val -4 - abs($val)}]
		set ext [string range $ext 0 $next]
	    }
	    set name [string range $name 0 $val]
	    set file $name..$ext
	}
	if {[string length $dir] > $to} {
	    set dir [string range $dir 0 $to]
	    set final [file join $dir... $file]
	} else {set final [file join $dir $file]}
    } else {set final $file}
    set final
}

proc formsocks {host port {userid ""}} {
    if {![FALC_isip $host]} {
	set host [dnslookup $host]
    }
    set version [format %c 4]
    set type [format %c 1]
    set portbin [binary format S1 $port]
    set hostbin [binary format I1 [FALC_ip2long $host]]
    set userid [expr {$userid == "" ? "[FALC_config connection user xircon]" \
	    : $userid}]
    set null [format %c 0]
    return $version$type$portbin$hostbin$userid$null
}

proc coloredstr {} {
    return "0zero 1one 2two 3three 4four 5five 6six\
	    7seven 8eight 9nine 10ten 11eleven 12twelve\
	    13thirteen 14fourteen 15fifteen"
}

proc mmss secs {
    if {$secs == -1} {
	return 0:00
    }
    set time [string trimleft [clock format [expr {$secs - 68400}] -format \
	    %M:%S] 0]
    set etime ""
    set is0 0
    foreach i [concat [expr {$secs/3600}] [split $time :]] {
	if {$i != 0} {set is0 1}
	if {$is0} {lappend etime $i}
    }
    set end [lindex $etime 0]
    foreach i [lrange $etime 1 end] {
	if {$i == ""} {set i 0}
	lappend end [ndig $i 2]
    }
    if {[llength $end] == 1} {set end [list 0 [lindex $end 0]]}
    return [join $end :]
}


####end of procs

loadmsg

####default popups


menu channel "*&Properties" {show_props channel [lindex [args] 0]}
menu channel ""
foreach i "status channel" {
    menu $i "C&onfigure->&Options->&Kano's options" {/config}
    menu $i "C&onfigure->&Options->&Modules' options" {/config mod}
    menu $i "C&onfigure->&Options->&Addons' options" {/config addon}
    menu $i "C&onfigure->"
    menu $i "C&onfigure->&Titlebar" /title
    menu $i "C&onfigure->&Flood protect" /flud
    menu $i "C&onfigure->Auto&join" /ajoin
    menu $i "C&onfigure->Spell &fix" /spellfix
    menu $i "C&onfigure->&Default messages" /defaults
    menu $i "C&onfigure->"
    menu $i ""
    menu $i "T&hemes->&List" {/theme *}
    menu $i "T&hemes->&Load" /theme
}

foreach i {channel status users query chat} {
    menu $i "&Addons->Load" /laddon
    foreach j [lsort $addglob] {
	menu $i "&Addons->Loaded->[file tail $j]" "/edit $j"
    }
    menu $i &Addons->
    menu $i ""
}

menu channel "Auto&Join->&Add current" {ajoin [channel] 1}
menu channel "Auto&Join->&Remove current" {ajoin [channel] 0}
menu channel "Auto&Join->"
foreach i {channel status} {
    foreach k [get_cookie nets] {
	foreach {name mask} $k {}
	foreach j [split [get_cookie ajoin($name)] ,] {
	    menu $i "Auto&Join->&$name->&[string range $j 1 end]" [concat \
		    /join $j]
	}
    }
}

foreach win "status channel query users chat" {
    menu $win "&Help->&Help" /help
    menu $win "&Help->&View help file" {
	if {[file exists kano.hlp]} {set file kano.hlp} elseif {[file \
		exists kano/kano.hlp]} {set file kano/kano.hlp} {
	    echo "[kano] Couldn't find help file (kano.hlp)!"
	    return
	}
	catch {FALC_shell winhelp -p $file}
    }
    menu $win "&Help->"
    menu $win "&Help->&Aliases->&Aliases" {/help aliases}
    menu $win "&Help->&Aliases->&Detailed list" {/help *}
    menu $win "&Help->&Aliases->"
    menu $win "&Help->&Hotkeys->&Hotkeys" {/help hotkeys}
    menu $win "&Help->&Hotkeys->"
    menu $win ""
    set pref "more options->"
    menu $win "${pref}performance &debug" {/c; /bench}
    menu $win "${pref}download kano &files" {grab_file {}}
    menu $win "${pref}view kano &news" {
	set forcemotd 1
	grab_motd
    }
    menu $win $pref
    menu $win "${pref}report a &bug" {bug_gui 0}
    menu $win "${pref}request new feature" {bug_gui 1}
    menu $win $pref
    menu $win "${pref}kano msgboard" {FALC_shell http://[get_cookie mykanourl \
	    www.kano.net]/board.cgi}
    menu $win "${pref}kano/web" {FALC_shell http://[get_cookie MyKanoUrl \
	    www.kano.net]}
    menu $win ""
}
menu query "&Whois++" {
    /whois [raw_args] [raw_args]
}
menu query "DNS &Lookup" {
    /dns [raw_args]
}

menu chat "*&Whois++"         { /wii [args] }

menu users "&Whois" {
    /whois [join [args] ,]
}
menu users "&Whois++" {
    foreach i [args] {
	/whois $i $i
    }
}
menu users "&DNS Lookup" {
    foreach i [args] {
	/dns $i
    }
}
menu users "*&Query" {
    /query [join [args] ,]
}

menu users ""
menu users "&Op->&Op" {
    massmode [channel] +o [args]
}
menu users "&Op->&Deop" {
    massmode [channel] -o [args]
}
menu users "&Op->O&pDop" {
    /opdop [ret [lindex [args] 0]]
}
menu users "&Op->"
menu users "&Op->&Voice" {
    /voc [arg]
}
menu users "&Op->D&evoice" {
    /dvoc [arg]
}
menu users "&Op->"
menu users "&Op->&Kick"   {
    masskick [channel] [args]
}
menu users "&Op->K&ickban" {
    set pr [prompt [kanovers] "Kick reason?"]
    /deop [raw_args]
    set m [list]
    foreach i [args] {
	lappend m [maskhost [uhost [join $i]]]
    }
    massmode [channel] +b $m
    masskick [channel] [args] $pr
}
menu users "&Op->&Temp Kickban" {
    set time [prompt [kanovers] "Ban for how long? (Min:Sec)" 0:30]
    set reason [prompt [kanovers] "Reason?"]
    foreach i [args] {
	/tkb [join $i] $time $reason
    }
    complete
}
menu users &Op->&PermBan {
    set m [prompt [kanovers] Reason? "You suck"]
    foreach i [args] {
	/perm $i $m
    }
}
menu users &Op->
menu users "&Op->Kickban NO&W" {
    /deop [raw_args]
    set m ""
    foreach i [args] {
	lappend m [maskhost [uhost [join $i]]]
    }
    massmode [channel] +b $m
    foreach {a b c d} [args] {
	/kick [channel] $a,$b,$c,$d
    }
}
####end of popups

loadmsg

proc cmdhist {} {
    set cmd [concat [string tolower [lindex [args] 0]] [lrange [split \
	    [raw_args]] 1 end]]
    history add $cmd
}
history keep 100
hotkey enter {
    set text [input get_text]
    if {[string match /* $text] && ![string match /!* $text]} {
	set sp [split $text]
	set cmd_history([string tolower [string range \
		[lindex $sp 0] 1 end]]) $text
    }
}
alias !* {
    set cmd [string tolower [string range [lindex [args] 0] 1 end]]
    if {[info exists cmd_history($cmd)]} {
#	echo "execing $cmd"
	say $cmd_history($cmd)
	complete
    } elseif {[llength [set find [array names cmd_history $cmd*]]]} {
#	echo "found $find"
	say $cmd_history([lindex $find 0])
	complete
    } else {
	echo "[kano] no commands matching /$cmd* in command history. try\
		pressing the UP key till you find what you're looking for."
    }
}

catch {file rename -force \
	addons/albare.km addons/albare.km.NO-LONGER-NECESSARY}
catch {file rename -force addons/help.km \
	addons/help.km.NO-LONGER-NECESSARY}
catch {file rename -force addons/procs.kf \
	addons/procs.kf.NO-LONGER-NECESSARY}
catch {file rename -force \
	addons/popups.ka addons/popups.ka.NO-LONGER-NECESSARY}

set firstload [clock clicks]
set kf [glob -nocomplain addons/*.kf]
set firstloads $kf
foreach i [lsort $kf] {
    loadmsg "Loading firstLoaders"
    set this_full_addon $i
    if {[catch {source $i} x] == 1} {msgbox $i $x}
    set this_full_addon {}
}
loadmsg
if {[llength $kf] != 0} {
    catch {addecho "[kano] Found [llength $kf] firstLoad[s [llength \
	    $kf]] in [file join [pwd] addons]"}
}
set firstload [expr {[clock clicks] - $firstload}]

benchmark 0
set verreply "[kclient xircon]\[b[version beta]\] - [kanoverz] by kano"
set origver $verreply

imsg ver

imsg env2
on notice {
    set d [lindex [args] 0]
    set l [string tolower $d]
    set m [my_nick]
    set n [string tolower $m]
    set o [string tolower [nick]]
    if {$l == $n} {set ischan 0} else {
	switch -- [string index $d 0] {
	    @ -
	    + {
		set char [string index $d 0]
		set d [string range $d 1 end]
	    }
	    default {set char ""}
	}
	set ischan 1
    }
    set l [string tolower $d]
}


on privmsg {
    set clix [clock clicks]
    set d [lindex [args] 0]
    set l [string tolower $d]
    set m [my_nick]
    set n [string tolower $m]
    set o [string tolower [nick]]
    if {$l == $n} {set ischan 0} else {
	switch -- [string index $d 0] {
	    @ -
	    + {
		set char [string index $d 0]
		set d [string range $d 1 end]
	    }
	    default {set char ""}
	}
	set ischan 1
    }
    set l [string tolower $d]
    isdebug init
}

proc @define@ args {
    error "Sorry, but you can't load Syntax themes."
}

imsg beforeload
#some FALCON stuff
if {[get_cookie falcdir] == ""} {set_cookie falcdir [file join [pwd] \
	falcon.dll]}
set falctime [clock clicks]
if {[catch {load [get_cookie falcdir]}]} {
    if {[catch {load falcon.dll}]} {
	set nofalc 1
    } else {
	set_cookie falcdir falcon.dll
    }
} else {
    set nofalc 0
    if {[FALC_ver] < 1.593} {set nofalc 1}
}
loadmsg
incr dlltime [expr {[clock clicks] - $falctime}]
imsg load

catch {rename FALC_xircreg falc_config}
proc FALC_config {key value {default DEF}} {
    if {$default == "DEF"} {falc_config $key $value} else {
	if {[catch {falc_config $key $value} val]} {return $default} else {
	    global FALC_config
	    if {[info exists FALC_config($key\\$value)]} {
		return $FALC_config($key\\$value)
	    } else {
		set FALC_config($key\\$value) $val
		return $val
	    }
	}
    }
}

catch {rename FALC_encrypt falc_encrypt}
catch {rename FALC_decrypt falc_decrypt}
proc FALC_encrypt {key string} {if {[catch {falc_encrypt $key $string} x]} \
	{return} else {return $x}}
proc FALC_decrypt {key string} {if {[catch {falc_decrypt $key $string} x]} \
	{return} else {return $x}}
catch {rename FALC_clpbrd FALC_clip}

## kcolor.dll
set kcolorloaded [expr {![catch {load kcolor.dll}]}]

if {![file exists [get_cookie readme $rm]]} {
    set x [recurse *read*me*.txt [pwd]]
    set file ""
    foreach i [string tolower $x] {
	set file $i
	if {[string first kano [file tail [file dirname $i]]] != -1 || \
		[file tail [file root $i]] == "README_IMPORTANT"} {
	    break
	}
    }
    set_cookie readme $file
}
set rm [get_cookie readme $rm]
if {[file exists $rm]} {
    set_cookie lastreadme [file mtime $rm]
    if {[file mtime $rm] > [get_cookie lastreadme 0]} {
	echo "[kano] Readme has changed since I last forced you to read it.."
	catch {FALC_shell -p $rm notepad}
    }
}

loadmsg
if {[catch {FALC_config connection user} i] || $i == 0} {set i xircon}
set_cookie ident(user) [get_cookie ident(user) $i]

benchmark
set unthemedata {
    #incorporate default.thm into the script:
    #non important ish.. 
    set vers werd
    set kanobg 15,01

    for {set i 0} {$i < 16} {incr i} {
	proc $i {} "return \003[format %.2d $i]"
    }

    set_cookie kano 14\[[15]k0a14\]

    #xdcc
    proc null args return
    proc startxdcc {} {global startingxdcc;set startingxdcc 0;return ""}
    startxdcc
    on dcc_complete {if {[string tolower [lindex [args] 0]] == "send"} \
	    {set_cookie crksent [expr {[get_cookie crksent 0] + [file size \
	    [lindex [args] 2]]}]}}
    proc b {} {return }
    proc u {} {return }
    proc o {} {return }
    proc bx {} {return [4]ù0í[4]ù[15]}
    #klepto stuff(here's where the broken code starts...)
    nt awaypubform {is gone[u],[o] [raw_args] [u]([u]l[b]![b]%msglog[u])[u]}
    nt awayform {[raw_args]  [u]([u]l[b]![b]%msglog[u])[u]}
    nt backform {has returned[u],[o] [raw_args] \
	    [u]([o]gone[b]/[b]%gone[u])[u]}

    nt xdccstart {[startxdcc]\[os%xdcc\] /ctcp [my_nick] \
	    xdcc send #N}
    nt xdcclist {[eval {
	if ![info exists startingxdcc] {set startingxdcc 0}
	incr startingxdcc [file size [lindex $i 0]]
	set fs [filesize [lindex $i 0] no %.1f]
	format "[b].[o][u].[b].([o][lindex $i 2][b]x[o][b][u]/[o][join $fs \
		{}][o][u][b])[o] [b]#[o]$count [u]-[o] [lindex $i 1]"
    }]}
    nt xdccend {\[ka%xdcc\][o] \
	    [b]o[o]ffered[b][u]/[o][filesize $startingxdcc][o] [b].[o] \
	    [b]s[o]ent[b][u]/[o][filesize [get_cookie crksent 0]]}

    proc flagc {nick chan {c1 ""} {c2 ""}} {
	set ch ""
	if {[isop $nick $chan]} {set ch @} elseif {[isvoice $nick $chan]} \
		{set ch +}
	if {$c1 == ""} {set c1 [10]}
	if {$c2 == ""} {set c2 0}
	return $c1$ch$c2$nick
    }

    nt socketfailed {[kano] [15]%type unable to create socket: %error.}
    nt rot14 {[13]<[o][15][nick]14:[13]ROT-14[13]>[o] [15][rot [string \
	    trimleft [join [lrange [args] 1 end]] §]]}
    nt ident {[kano] 14([o][12]%host14)[o][15] requested ident[b]/[o \
	    ]returned 14([o][12][get_cookie ident(user)]14)[o]}
    nt identoff {[kano] 14([o][12]%host14)[o][15] requested ident[b]/[o \
	    ][15]ignoring}
    nt page {[kano] [12]page[o] 14([o][12][user]@[host][o]14)[o \
	    ] paged you!}
    nt sound {[kano] [13]([lrange [split [lindex [args] 2] \\] end \
	    end]) 0ð[15] [nick][15] [lrange [args] 3 end]}
    nt chanwallops {[u]\[[o]ka[u]%[o]wall[u]/[o][channel][u]\][o] [raw_args \
	    ]}
    nt chanwallx {[u]\[[o]ka[u]%[o]wallx[u]/[o][channel]:[nick2nick [lindex \
	    [args] 0]][u]\][o] [join [lrange [args] 1 end]]}
    nt myctcp {[kano] [2]ctcp 14([o][12][nick2nick [join [lindex [args] \
	    0] ,]]14)[o][15] [string toupper [join [lindex [args] \
	    1]]] [join [lrange [args] 2 end]]}
    nt mysound {[13]([lrange [split [lindex [args] 0] \\] end \
	    end])0 ð[15] [my_nick][15] [lrange [args] 1 end]}
    nt autoignoreall {[kano\
	    ] Possible flood detected: added *!*@* to ignore list for 30s.}
    nt autoignore {[kano] %type flood from [nick] 14\[[15][user\
	    ]@[host]14\][15]; ignoring for 30secs.}
    nt unignore {[kano] Autoignore expired: Unignoring %host.}
    nt dictsearching {[bx] Searching dictionary server for: [raw_args]}
    nt dicterr {[bx] Dictionary server error: %dict}
    nt dicterrnotfound {[bx] Dictionary: %word not found; no suggestions}
    nt dicterrfound {[bx] Spelling error in %word. Suggestions:}
    nt dictfound {[bx] Definition for %word:}
    nt dictdef {[bx] %gets}
    nt dictunknown {[bx] %gets}
    nt tempkickban {[bx] Unbanning [join [lindex [args] 0]] in [lindex \
	    [args] 1].}
    nt tkbunban {[bx] TempKB Expired: Unbanning %host on %chan.}
    nt topiclock {[kano] 14\[[10][channel]14\][15] Topic Locked.}
    nt topicunlock {[kano] 14\[[10][channel]14\][15] Topic [u]Un[o]locked}
    nt addperm {[bx] Added %nick 14([15]%user14)[15] to permban.}
    nt remperm {[bx] Removed %nick 14([15]%user14)[15] from permban.}

    ###basic stuff
    nt dcctext {14<[12][nick][o]14>[o] [lindex [args] 0]}
    nt dccact {14\[[o][2][nick]14([o][12]act14)[o]14\][o] [lrange \
	    [lindex [args] 0] 1 end]}
    nt myaction {[2]øùú [12][my_nick][15] %text}
    nt action {[2]øùú [12][nick] [o][15] %text}
    nt actionnoquery {[kano] [12][nick] [15]%text}
    nt pubmsg {14<[flagc [nick] [lindex [args] 0]]14>[o][15] %text}
    nt mymsg {[10]<[flagc [my_nick] [lindex [args] \
	    0] [12] [11]][10]>[15] %text}
    nt highlight {[10]<[flagc [nick] [lindex [args] \
	    0] [12]][10]>[15] %text}
    nt mymsgnoquery {[12]\[[10]m[11]sg[12]([11]%nick[12])\][o][15] %text}
    nt msg {[12]<[o][15][nick][12]>[o][15] %text}
    nt msgnoquery {[12]\[[11][nick][12]([10][user]@[host][12])\][o][15] %text}
    nt pubnotice {14-[13][nick][6][lindex [args] 0]14-[15] %text}
    nt servnotice {[9][iph [connected] [9]\[[lindex [set split [split \
	    [lindex [server] 0] .]] [expr {[llength $split]-2}]]\] \
	    ""]![3][lindex [server] 0][15] %text}
    nt notice {[2]-[12][nick][o][2]-[15] %text}
    nt mynotice {14\[[o][5]notice014([4][nick2nick [join [lindex \
	    [args] 0]]]14)\][15] %text}
    nt mynoticenoquery {[t mynotice]}

    nt joincountry {}
    nt join {[kano] [2]join 14([12][lindex [args] \
	    0]14)[o] [o][nick][o] 14([12][user]@[host]14)[o]}
    nt myjoin {[t join]}

    nt part {[kano] [2]leave 14([12][lindex [args] \
	    0]14)[o] [nick][o] 14([12][user]@[host]14)[o]}
    nt quit {[kano] [2]signoff [o][nick] 14([o][arg]14)[o]}
    nt kick {[kano][o] [2]kick14([12][lindex [args] 0]14) [o][lindex \
	    [args] 1] 14([o][lindex [args] 2]14)[o] by [nick]}
    nt kickedyou {[kano][o] [2]kick14([12][lindex [args] \
	    0]14) [11]YOU! 14([15][lindex [args] 2]14)[15] by [nick]}
    nt modechange {[kano] [2]mode 14([o][12][lindex [args] \
	    0]14)[o] [15][lindex [args] 1]%mode 14([o][12]%nick14)}

    nt mynick {[kano] [2]nick14([12][my_nick\
	    ]14)[o] to 14([12][arg]14)}
    nt nick {[kano] [2]nick14([12][nick]14)[o] to 14([12][string \
	    trimleft [raw_args] :]14)}
    nt wallops {[kano] wallops: [arg]}
    nt dccsend {}
    #nt dccsend is not needed.. enuff with the dcc hooks
    #nt dccsend {[kano] [2]sending 14([o][12][lindex [args] \
	    2]14)[o] to 14([o][12][lindex [args] 1]14)[o]}
    nt dccreq {14\[[o][10]dcc[o]14/[o][11]GET[11] [11][file tail \
	    [lindex [args] 2]]14\][o] 14([o][12][lindex [args] \
	    1]![12][uhost [lindex [args] 1]][2]:[o][12][filesize \
	    [lindex [args] 3]]14)[o]}
    nt dccbegin {14\[[o][10]dcc[o]14/[o][11][lindex [args] \
	    0]14\][o] established 14([o][12][lindex [args] \
	    1]![12][lindex [split [uhost [lindex [args] 1]] @] 1]:[rand \
	    1000 6000]14)[o]}
    nt dccdone {14\[[o][10]dcc[o]14/[o][11][lindex [args] 0] [11][file \
	    tail [lindex [args] 2]]14\][o] [filesize [file size [lindex \
	    [args] 2]]] 14([o][12][expr {[lindex [args] 0] == "SEND" ? \
	    "to" : "from"}] [12][lindex [args] 1] [12][lindex [args] \
	    4]kb/sec14)[o]}
    nt dccerr {[kano] [15]Error14:[15 \
	    ] DCC connection closed 14([15][lindex [args] \
	    2]14)[15] [iph {[lindex [args] 0] == {SEND}} to from] [lindex \
	    [args] 1].}
    nt getnick {[kano] Regaining nick [arg].}
    nt getnickerr {[kano] You are already known as [nick]!}
    nt askopschan {[kano] Asking %bots for %opsorauth.}
    nt askopsmain {[kano] 14\[[10]%chan14\][15 \
	    ] Asking %bots for %opsorauth}
    nt cycleforops {[kano] Cycling %chan to gain ops.}
    nt newconn {[bx] Attempting connection to [10][lindex [args] \
	    0][o] on port [10][lindex [args] 1][o]}
    nt closedconn {[kano] Connection closed.}
    nt tclconsoleinput {[13]% [join [lrange [args] 1 end]]}
    nt notifyon {[kano] [2]signon 14([12][nick]![user]@[host]14)[o \
	    ] at 14([12][clock format [unixtime] -format %I:%M][string \
	    tolower [clock format [unixtime] -format %p]]14)}
    nt notifyoff {[kano] [2]signoff 14([12][nick]![user]@[host]14)[o \
	    ] at 14([12][clock format [unixtime] -format %I:%M][string \
	    tolower [clock format [unixtime] -format %p]]14)}
    nt invitedyou {[kano] [o][nick] invites you to 0[lindex [args] 1]
    ka[u]n[o]o[u]([o]v[lindex [kanovers] 1][u])[o] Press CTRL-I to join}
    nt ctcpreply {[kano] [2]ctcp 14([o][12][lindex [args] \
	    1]14)[o] [nick] 14([o]%reply14)[o]}
    nt ctcptimereply {[kano] [15]CTCP 0[lindex [args] \
	    1][15] reply from [format %-9s [nick]]: %reply}
    nt ctcpcloaked {[kano] [2]ctcp 14([o][12][user]@[host]14)[o] [lindex \
	    [args] 1] 14([o][12][lindex [args] 0][15]/[o][12]cloaked14)[o]}
    nt xdcc {14\[[o][10]dcc[o]14/[o][11]REQ[o]14\][o] [nick \
	    ] 14\[[o][lindex [args] 1] [lindex [args] 2][11]14\][o]}
    nt ctcp {[kano] [2]ctcp 14([o][12][nick]![user]@[host]14)[o] [15 \
	    ]%cmd 14([o][12][lindex [args] 0]14)[o]}

    ###raw stuff
    ###woo wish this stuph really mattered for 80% of irc *cough* lamers 
    ###*cough*
    nt stats {[join [lrange [args] 1 end]]}
    nt motd {[bx] [lrange [args] 1 end]}
    nt dline {[kano] Line: [lrange [args] 1 end]}
    nt umode {[kano] [2]umode 14([o][12][lindex [args] \
	    1]14)[o] [12][my_nick]}
    nt lusercrap {[kano] [join [lrange [args] 1 end]]}
    nt admincrap {[kano] [join [lrange [args] 1 end]]}
    nt overloaded {[kano] [lrange [args] 1 end]}
    nt silenced {[kano] [lrange [args] 1 end]}
    nt killed {[kano] [15][string trimright [lindex [split [arg]] 8] \
	    .][15] was killed by [15][lindex [split [arg]] 10][15] [join \
	    [lrange [split [arg]] 13 end]]}
    nt trace {[kano] [2]trace 14([o][12][lindex [args] 1] [12][lrange \
	    [args] 1 end]14)[o]}
    nt line {[kano] Line: [lrange [args] 1 end]}
    #nt away == no such thing in osiris.bx... no such thing in osiris.thm
    nt away {}
    nt notaway {[kano] You are no longer marked as being away.}
    nt helpful {[kano] [lrange [args] 1 end]}
    ####WHOIS STUFF
    #blame klepto for broken code klepto(deviant@lords.com)
    nt wwnick {[12]Ú[2]Ä14ÄÄÄÄ[15]ù [15]ú 0.
[2]³[o] 0[lindex [args] 1]  0. [o][2][lindex [args] 2]@[lindex [args] \
	3][o] 14([o][country [lrange [split [lindex [args] 3] .] end \
	end]]14)[o]}
    nt wwaddr {}
    nt wiaddr {}
    nt wisign {}
    nt wwname {14|[o] 0r[15]ealname  [12]í[o] [lindex [args] 5]}
    nt wwend {[12]À[2]Ä14ÄÄÄÄ[15]ù [15]ú [o]. 0ú [2]ø}
    nt winame {14|[o] 0r[15]ealname  [12]í[o] [lindex [args] 5]}
    nt witop {}
    nt winick {[12]Ú[2]Ä14ÄÄÄÄ[15]ù [15]ú 0.
[2]³[o] 0[lindex [args] 1]  0. [o][2][lindex [args] 2]@[lindex [args] \
	3][o] 14([o][country [lrange [split [lindex [args] 3] .] end \
	end]]14)[o]}
    nt wichan {14:[o] 0c[o][15]hannels [2]ì[o] 15[lindex [args] 2]}
    nt wiserv {14. 0s[15]erver   14øù.14 [2][lindex [args] \
	    2][o] 14(15[lindex [args] 3]14)[o]}
    nt wiaway {  0a[15]way       [2]ú[o] [lindex [args] 2]}
    nt wiidle {  0i[15]dle        [12]í 15[since [lindex [args] \
	    2]] (signon: [since c[lindex [args] 3]] ago)}
    nt wioper {  0o[15]perator    14+[o] [15 \
	    ]is *not* an ereet packet warrior!}
    nt wiend {[12]À[2]Ä14ÄÄÄÄ[15]ù [15]ú [o]. 0ú [2]ø}
    # end of klepto's broken code
    #the real meat of osiris.thm
    #here's where the broken shit starts..
    nt isaway {[kano] 0[lindex [args] 1][15] is away14:[15] [lindex \
	    [args] 2]}
    nt ismode {[kano] [2]mode 14([o][12][lindex [args] \
	    1]14)[o] 14([o][12][lindex [args] 2][12] [12][lindex [args] \
	    3]14)[o]}
    nt notopic {[kano][10] [lindex [args] 1][15]: no topic set. }
    nt istopic {[kano] [2]topic [2]for 14([o][12][lindex [args] \
	    1]14)[o] 14([o][lindex [args] 2]14)[o]}
    nt settopic {[kano] [2]topic [2]by 14([o][12][lindex [args] \
	    2][o]14)[o] 14([o][clock format [lindex [args] 3]]14)[o]}
    nt topicchange {[kano] [2]topic 14([o][12][lindex [args] \
	    0]14)[o] [lindex [args] 1][o] 14([o][12][nick]14)[o]}
    nt topiclocked {[kano] Channel topic locked. Switching back...}
    nt inviting {[bx]0 Inviting [lindex [args] 1] to [lindex [args] 2]}
    nt summoning {[kano] Summoning [lindex [args] 1] to IRC}
    nt atversion {[kano] [lindex [args] 2] is at version [lindex [args] \
	    1][lindex [args] 3]: [lindex [args] 4]}
    nt who {[kano] [2]$4 14([o][12]$5 [12]$6  [12]$1[2]@[o][12]$214)[o \
	    ] 14([o]$714)[o]}
    nt bans {[kano] [2][format %-9s [lindex [args] 1]] [o][12][format \
	    %-14s [lindex [split [lindex [args] 3] !] \
	    0]][o] 14([o][12][lindex [args] \
	    2]14)[o]      14([o][since c[lindex [args] \
	    4]]14)[o]}  
    nt nowoper {[bx] You are now an IRCop}
    nt time {[kano] [lindex [args] 1] [lindex [args] 2]}
    nt user {[kano] [lrange [args] 1 end]}
    nt nosuchnick {0[lindex [args] 1][15]: No such nick/channel}
    nt nosuchserver {[kano][15] [lindex [args] 1] no such server}
    nt invalidchan {[kano] [lindex [args] 1]: invalid channel}
    nt cannotsendmoderated {[kano][15] [lindex [args] \
	    1]  Cannot send to channel}
    nt toomanychans {[bx] [2]channel 14([12][lindex [args] \
	    1]14)[o] [o]get a life! *too many channels*}
    #some shit changed... most of this bs wasn't
    nt wasnonick {[kano][15] [lindex [args] 1]  There was no such nickname}
    nt occurs {[kano][15] [lindex [args] \
	    1] occurs more than once; no message sent}
    nt pingorigin {[kano] No ping/pong origin specified}
    nt notext {[bx] No text to send.}
    nt notoplvl {[kano][15] [lindex [args] 1]: No toplevel domain specified.}
    nt topwild {[kano][15] [lindex [args] \
	    1]: Wildcard in toplevel domain (can't do that!)}
    nt cmdunknown {[kano][15] Unknown command: [string tolower [lindex \
	    [args] 1]]}
    nt noadmin {[kano][15] [lindex [args] 1] No admin information.}
    nt nonickgiven {[kano] No nickname given.}
    nt badnick {[kano][15] Nickname specified is illegal}
    nt nickinuse {[kano] [2]nick 14([12][lindex [args] \
	    1]14)[o] [12]is taken..}
    nt nickcollision {[kano] Nickname collision.}
    nt notonchan {[kano] [lindex [args] 2]: [15][lindex [args] \
	    1] isn't on channel.}
    nt notinchan {[kano] [lindex [args] 1] you're not in that channel.}
    nt alreadyonchan {[kano][15] [lindex [args] 1]14\\[10][lindex [args] \
	    2][15] already on channel.}
    nt notlogged {[kano] Sorry, [15][lindex [args] \
	    1] isn't logged on; cannot summon him.}
    nt summondisabled {[kano] Sorry, summon has been disabled.}
    nt usersdisabled {[kano] Users has been disabled.}
    nt notregistered {[kano] You have not registered.}
    nt toofewparms {}
    nt alreadyregistered {[kano] You've already registered.}
    nt notprivileged {[kano] Your host is not among the privileged.}
    nt badpass {[kano] Incorrect password.}
    nt bannedfromserver {[kano] Can not connect to server: k-lined.}
    nt keyset {[kano][15] [lindex [args] 1]'s channel key already set.}
    nt chanfull {[kano] [15]\[[lindex [args] 1]\]  Channel is full}
    nt chaninvite {[kano] [15]\[[lindex [args] 1]\] Invite only channel.}
    nt chanbanned {[kano] [15]\[[lindex [args] 1]\] banned from channel.}
    nt chanbadkey {[kano] [15]\[[lindex [args] 1]\] Bad channel key}
    nt banlistfull {[kano] \[[15][lindex [args] 1]\] banlist is full.}
    nt nopermission {[kano] Permission denied; you're not an IRCop.}
    nt notop {[kano][15] You're not opped on [lindex [args] 1][iph [isop \
	    [my_nick] [join [lindex [args] 1]]] ", but you are (desync)" ""]}
    nt unknownmode {[kano][15] Unknown MODE flag.}
    nt serverkill {[kano] You can't kill a server.}
    nt notoper {[kano] you don't have any Olines idiot.. \
	    give head to an oper today [12][my_nick]}
    nt notyourmode {[kano][15] Can't change modes for other users.}
    nt silencefull {[kano][15] Your silence list is full.}
    nt nogline {[kano][15] [lindex [args] 1][15] No such G-line.}
    nt created {}
    #nt created == lame
    proc chanecho {chan text} {
	if [ison [my_nick] $chan] {
	    echo $text channel $chan
	} {
	    echo $text
	}
    }

    nt users {[eval {
	set chan [lindex [args] 2]
	set users [split [lindex [args] 3]]
	set ops ""
	set voc ""
	set non ""
	foreach i [lsort $users] {
	    switch [string index $i 0] {
		@ {lappend ops $i}
		+ {lappend voc $i}
		default {lappend non $i}
	    }
	}
	chanecho $chan "    [15]0ÚÄÄ[11]ÄÄÄ[10]ÄÄ[11]ÄÄ[10]ÄÄÄÄÄ14Ä[10 \
		]ÄÄ14ÄÄ[10]Ä14ÄÄÄ[10]Ä14ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ[10 \
		]-14\[0[15] [10]o14/[0][format %2s [llength \
		$ops]]0[15] 14ú0[15] [10]v14/0[format %2s \
		[llength $voc]]0[15] 14ú0[15] [10]n14/[0][format \
		%2s [llength $non]]0[15] 14][10]-14ÄÄ¿"
	chanecho $chan "  [10]ÚÄÄ-14->[format %63s ""]14<-[10]-ÄÄ¿"
	foreach {a b c d e} [concat $ops $voc $non] {
	    set echo "  14³ [10]³    "
	    foreach i {a b c d e} {
		set m [set $i]
		if {$m == ""} {set z " "} {set z ÷}
		set m [10][format %-10s $m]
		set m [rep $m @ 0@[10]]
		set m [rep $m + 0+[10]]
		append echo "14$z $m "
	    }
	    append echo "[10]³ 14³"
	    chanecho $chan $echo
	}
	chanecho $chan "  [10]ÀÄÄ-14Ä>                           0t[15 \
		]o14tal ([o][15][format %3s [llength \
		$users]]14)                         14<Ä[10]-ÄÄÙ"
	chanecho $chan \
		"    14ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ[10 \
		]Ä14ÄÄÄ[10]Ä14ÄÄ[10]ÄÄ14Ä[10]ÄÄÄÄÄÄ[11]ÄÄ[10]ÄÄ[11 \
		]ÄÄÄ0ÄÄÙ"
    }]}
    ###added by kano
    nt split {[kano] [2]split 14([12][lindex [set servs [split [string \
	    range [raw_args] 1 end]]] 0]14)[o] 14([12][lindex $servs \
	    1]14)[o]\nPress CTRL-F to view split users}
    nt rejoin {[kano] [2]rejoin 14([12][lindex $splitwin($swl) \
	    2]14)[o] 14([12][lindex $splitwin($swl) 1]14)[o]}
    nt hashed {[kano] [2]hash 14([12][lindex [args] \
	    1]14)[o] ${t}sec 14([12][llength [chanlist [lindex [args] \
	    1]]]14)[o]}

    nt wallmsg {14<[flagc [nick] [lindex [args] 0]][10]:[11][lindex \
	    [args] 0]14>[o][15] %text}
    nt wallnotice {[t pubnotice]}
    
    nt dnslookup {[kano] looking up $host}
    nt dnsrelay {[kano] [concat ${litehelp}$dnsreq($request) \
	    ${darkhelp}([listify $m0])] -> [concat ${litehelp}$request \
	    ${darkhelp}([listify $m1])] -> [concat ${litehelp}$result \
	    ${darkhelp}([listify $m2])] \[^G=copy\]}
    nt dnsfound {[kano] resolved [concat ${litehelp}$request \
	    ${darkhelp}([listify $m1])] to [concat ${litehelp}$result \
	    ${darkhelp}([listify $m2])] \[^G=copy\]}
    nt dnsrevfail {[kano] resolved [concat ${litehelp}$dnsreq($request) \
	    ${darkhelp}([listify $m0])] to [concat ${litehelp}$request \
	    ${darkhelp}([listify $m1])], reverse lookup failed \[^G=copy\]}
    nt dnsfail {[kano] could not resolve [concat $request \
	    ${darkhelp}([listify $m1])]}    

    proc defthm {} {
	global user
	set end ""
	set ig [get_user $user ignore]
	set in [get_user $user notes]
	set it [get_user $user notify]
	if {$ig != ""} {lappend end "ignore: $ig"}
	if {$it == "Y"} {lappend ind notify}
	if [catch {llength $in}] {return}
	foreach i $in {
	    if [catch {llength $i}] continue
	    set j [lindex $i 0]
	    if {$j == "perm"} {lappend end permban}
	    if {$j == "protect"} {
		lappend end "prot: [join [lindex $i 1] ", "]"
	    }
	    if {$j == "bot"} {lappend end bot}
	}
	if {$end != ""} {
	    return ": [join $end /]"
	} {return}
    }
    nt ujoin {[t join] 14(user $mask[defthm]14)}

    nt wwend {} ;#whowas end (369)
    nt banend {} ;#banlist end (368)
    nt whoend {} ;#who end (315)
    nt nameend {} ;#names end (366)
    nt timestamp {[kano] [2]timestamp [string trimleft [string tolower \
	    [cform "%I:%M%p 14([12]%d%h%y14)"]] 0]}
    nt dccs {$dccx(file)\[[filesize $dccx(upto)]/[filesize \
	    $dccx(size)]([format %.1f [expr {($dccx(upto) / \
	    double($dccx(size))) * \
	    100}]]%)\]: $dccx(nick)/$dccx(pass) @$dccx(rate)k/s}
    nt except {[t bans]}
    nt exceptend {[t banend]}
    nt mydccmsg {[t mymsg]}
    nt mymsgquery {[t mymsg]}
}
if {[info exists identerr]} {
    set id $identerr
    unset identerr
}
loadmsg "Loading theme"
eval $unthemedata
if {[get_cookie theme] != ""} {
    set themeloaded [loadtheme [get_cookie theme]]
} else {set themeloaded 0}
if {[info exists id]} {
    set identerr $id
    unset id
}
loadmsg

###END
set llength [llength [array names help]]
set aliasz ""
set aliasm ""
set mz 0
set helpstuff ""
###
if 0 {
source mkhln.tcl
###
}
set help() [subst {!
${darkhelp}ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
${darkhelp}³ [format %31s {}]${darkhelp}kano.tcl ${darkhelp}³
${darkhelp}³ ${litehelp}There are two main help           ${darkhelp}by   \
	${darkhelp}³
${darkhelp}³ ${litehelp}categories:                    ${darkhelp}  kano  \
	${darkhelp}³
${darkhelp}³                                         ³
${darkhelp}³ ${litehelp}1. Aliases        /help aliases         ${darkhelp}³
${darkhelp}³ ${litehelp}2. Hotkeys        /help hotkeys         ${darkhelp}³
${darkhelp}ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
}]
set help(hotkeys) [subst {!
         ${darkhelp}ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
         ${darkhelp}³ ${litehelp}Kano Hotkeys: ${darkhelp}³ ³\
		 ${litehelp}Emacs Hotkeys: ${darkhelp}³
${darkhelp}ÚÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄ¿
${darkhelp}³ ${litehelp}F2   ${litehelp}Autoreply         ${darkhelp}³ ³\
	${litehelp}C-e  ${litehelp}End-of-line       ${darkhelp}³
${darkhelp}³ ${litehelp}F3   ${litehelp}Inline-Nick-Comp  ${darkhelp}³ ³\
	${litehelp}C-a  ${litehelp}Beginning-of-line ${darkhelp}³
${darkhelp}³ ${litehelp}F4   ${litehelp}Flip              ${darkhelp}³ ³\
	${litehelp}C-k  ${litehelp}Kill-rest-of-line ${darkhelp}³
${darkhelp}³ ${litehelp}F5   ${litehelp}Flip #2           ${darkhelp}³ ³\
	${litehelp}C-w  ${litehelp}Kill-start-of-line${darkhelp}³
${darkhelp}³ ${litehelp}F6   ${litehelp}Join-Last-Split   ${darkhelp}³ ³\
	${litehelp}C-t  ${litehelp}Transpose chars   ${darkhelp}³
${darkhelp}³ ${litehelp}TAB ${litehelp}Nick Comp/Autoreply${darkhelp}³ ³\
	${litehelp}C-y  ${litehelp}Paste             ${darkhelp}³
${darkhelp}³ ${litehelp}C-i  ${litehelp}Join-Last-Invites ${darkhelp}³ ³\
	${litehelp}C-b  ${litehelp}Back-char         ${darkhelp}³
${darkhelp}³ ${litehelp}C-z  ${litehelp}ALL CAPS          ${darkhelp}³ ³\
	${litehelp}C-f  ${litehelp}Forward-char      ${darkhelp}³
${darkhelp}³ ${litehelp}A-z  ${litehelp}all lowercase   ${darkhelp}ÚÄÁÄÙ\
	${litehelp}C-s  ${litehelp}Forward-search    ${darkhelp}³
${darkhelp}ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ${darkhelp}´ ${litehelp}C-r     \
	${litehelp}Backward-search   ${darkhelp}³
                       ${darkhelp}³ ${litehelp}C-x C-c \
			       ${litehelp}Exit-client       ${darkhelp}³
                       ${darkhelp}³ ${litehelp}C-x ^   \
			       ${litehelp}Maximize-window   ${darkhelp}³
                       ${darkhelp}³ ${litehelp}C-z     \
			       ${litehelp}Minimize-window   ${darkhelp}³
                       ${darkhelp}³ ${litehelp}C-x k   \
			       ${litehelp}Close-window      ${darkhelp}³
                       ${darkhelp}³ ${litehelp}C-x o   \
			       ${litehelp}Switch-windows    ${darkhelp}³
                       ${darkhelp}³ ${litehelp}C-x m   \
			       ${litehelp}Check-mail        ${darkhelp}³
                       ${darkhelp}³ ${litehelp}C-q ### \
			       ${litehelp}                  ${darkhelp}³
                       ${darkhelp}³ ${litehelp}    x   \
			       ${litehelp}Quote-character   ${darkhelp}³
                       ${darkhelp}ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
${litehelp}Get specific help: 15/help <hotkey>
${litehelp}Get emacskeys help: 15/help emacs \[only if you have emacs.ka\]
}]
kaddhelp +f2 "This will put '[ncomp NickName] ' in the input line, where\
	NickName is the last person that said your nickname in a channel.\
	Keep pressing F2 to scroll through nicks." ""
kaddhelp +f3 "I just did this to be cool. =) For example, if 'kano' is on\
	the channel, type 'ka' in the input box then hit F3, see what\
	happens. =)" ""
kaddhelp +f4 "Like the /flip command. Do it and see. Now also works with\
	highlighted text." "`/flip"
kaddhelp +f5 "Like the /flip2 command. Do it and see." "`/flip2"
kaddhelp +tab "This is seriously pretty useful. in a channel, type the\
	first few letters of someone's nick, anywhere in on the line,\
	then move the cursor anywhere in the word or on either \"edge\"\
	and press TAB. it completes the nickname for you. Try it! NEW:\
	When the cursor is at the front of the line inside an alias,\
	it will show you a list of possible completions. For an example,\
	type /wa then press TAB. Cool, eh? =) OR, if the command line is\
	completely empty, it works as BX-style autoreply." ""
kaddhelp +c-i "\[Control+I\] Inserts '/join #x', where #x is the last\
	channel you were invited to, into the command line. You can cycle\
	through the channels you were invited to by pressing Ctrl+I again." \
	""
kaddhelp +c-z "\[Control+Z\] Turns selected text (or the whole line if\
	nothing is selected) into ALL CAPS." "`A-z"
kaddhelp +a-z "\[Alt+Z\] Turns selected text (or the whole line if nothing\
	is selected) into all lowercase." "`C-z"
set nohelp "aliases hotkeys {} features"
loadmsg
#config
#ac <type> <get script> <set script> <desc ...>
#
#type: check, edit, combo, nedit (numbers only), ecombo (editable)
#get script: the script to exec to get the initial value (or "@<cookie>" to
#            set a cookie to the value
#set script: the script to exec after the user hits OK (or "@<default
#            value>" if you used "@<cookie>" for get script
#desc: duh

ac combo {
    if {$return != ""} {
	set index [expr {int([lindex [split $return] 0] -1)}]
	set file [lindex $RECURSE $index]
	/theme $file
    }
} {
    set RECURSE [lsort [recurse *.thm [pwd]]]
    set temp [list [list]]
    foreach i $RECURSE {
	lappend temp [concat [llength $temp]. [rep [file root [local $i]] / \
		\\]]
    }
    set temp
} Change themes:
ac edit @umode @ Default usermode:
ac check @msglog @ Message Log
ac check @bans(enf) @ Kick banned users
ac check @bans(self) @ Unban self
ac combo {
    switch -glob $return {
	N* {set_cookie hilite(switch) 0}
	W* {set_cookie hilite(switch) 1}
	E* {set_cookie hilite(switch) 2}
    }
} {
    set 0 Nothing
    set 1 {Window switch}
    set 2 {Echo in focused}
    switch [get_cookie hilite(switch)] {
	0 {set hl [list $0 $1 $2]}
	1 {set hl [list $1 $0 $2]}
	2 {set hl [list $2 $0 $1]}
    }
    return @$hl
} On nick highlight:
ac check @motd @ Show server MOTD
ac check {
    if {$return} {set_cookie status ""} {set_cookie status status}
} {
    if {[get_cookie status] == "status"} {return  @0} {return  @1}
} Serv. messages in active window
ac ecombo {
    if {$return == "off"} {set return 0}
    set_cookie autoaway $return
} {return  "@[get_cookie autoaway] off"} Autoaway (mins.)
ac check @shownames @0 Show names list on join
ac check @autocloak @ Ignore CTCP's while away
ac edit @awaynick @ Append to nick while away:
ac check @autoresetaway @0 Auto reset away timer
ac nedit @nicklength @9 Nickname length
ac nedit @echolines @100 Restore lines in rebuilt window:
ac check @suppress_mode @1 Suppress repeated modes
ac edit @newmode @stn Set modes on channel creation:
ac check @usenickcomp @1 Use nick completion
ac check @ident(on) @ Use ident server
ac edit @ident(user) @user Ident username
ac check @suppress_text @0 Suppress repeated channel text
ac check @timestamp @1 Hourly timestamp
ac check @jpqstatus @0 Join/part/quit in status
ac check @showadd @1 Show addons in version reply
ac check @cycle @1 Cycle channels for ops if empty
ac check @autogetnick @1 Attempt to regain default nick if in use
ac nedit @modenum @4 Number of modes per line
ac check @pron @0 Pronounce numbers where possible
ac check @ajoin_ok @1 Use autojoin
ac check @ujoin @1 Detailed userinfo on join (useful!)
ac nedit @linesec @20 Maximum lines of text per second
ac nedit @kfilesize @51200 Display kFile status at..
ac check @dcccloak @0 Ignore DCC requests if cloaked
ac check @extendmotd @1 Use extended kanoMOTD
ac check @ajoin2 @0 Autojoin chans in all server windows (/newserver)
ac check @kfilewin @1 kanoFile creates own window
ac check @autobug @1 Report fatal errors to script author
ac check @mapnickserv @0 Map /msg nickserv to /raw nickserv
ac edit @compchar @: Nick completion character
ac edit @titlebar @ Titlebar text
ac check @kanonewsbox @1 Show kanoNews dialog box
ac check @show_ident @1 Show ident requests in status window
ac check @quick_cookie @1 Cache configuration values (faster, but data may be \
	lost on occasion)
ac check @showawaymsg @1 Show public away messages

set addtime [clock clicks]
if {$addglob != ""} {
    foreach i [lsort $addglob] {
	loadmsg "Loading addons"
	set this_addon [file root [file tail $i]]
	set this_full_addon $i
	if {[catch {source $i}] == 1} {
	    addecho "[kano] Error loading addon '$i':\n$errorInfo"
	    lappend addErrors $this_full_addon
	}
    }
    set this_addon {}
    set this_full_addon {}
}
set t [expr {[clock clicks] - $addtime}]
if {[llength $addglob]} {set end " loaded in ${t}ms ([expr {$t/[llength \
	$addglob]}] per addon)"} else {set end ""}
addecho "[kano] Found [llength $addglob] addon(s) in [string tolower [file \
	join [pwd] addons]]..$end"
set loadaddons $t


#ap <alias> <reply [reply reply reply etc]> <stop-redirecting reply>
ap whois 310 311 312 313 317 319 318
ap who 352 315
ap names 353 366
ap bans 367 368
ap mode 324
ap whowas 314 319
ap lag pong
ap topic 332 333

foreach i [array names addpipe] {
    on [lindex $addpipe($i) 0] {
	if [info exists delredir([event])] {
	    foreach j $delredir([event]) {
		catch {unset isredir($j)}
	    }
	    unset delredir([event])
	}
    }
}
#IAL stuff if auto-ial is off (which it isn't :)
foreach i "ctcp ctcp_reply chat_connect chat_accept chat_text chat_send \
	chat_disconnect whois mode dcc_create dcc_rename dcc_begin \
	dcc_complete dcc_error notify denotify privmsg notice join part \
	quit kick" {
    on $i {set who([string tolower [nick]]) [user]@[host]}
}
catch {set origscroll [FALC_lock scroll]}
benchmark
if {[info exists env(KMOTD)]} {
    if {![catch {eof $env(KMOTD)} x] && !$x} {
	catch {fileevent $env(KMOTD) readable "kanomotd $env(KMOTD)"}
    }
}
benchmark
after idle {set old_fullver $verreply}

if {[get_cookie askdcc] == ""} {set_cookie askdcc 1}
#GET kano {E:\XIRCON\aergfg} 10 0.0000 0
benchmark

foreach i {quit part nick} {
    on $i {
	if {[string tolower [nick]] == [string tolower $getnick] && [array \
		names splitwin] == ""} {
	    /nick $getnick
	}
    }
}
benchmark
if {[get_cookie title] == ""} {set_cookie title "today client os time for \
	away lag unf"}

benchmark

foreach i [get_cookie tzones] {
    set tzt([string tolower [lindex $i 0]]) $i
}
benchmark
set countryl(a) [list af al dz as ad ao ai aq ag ar am aw au az bs bh bd bb \
	by be bz bj bm bt bo ba bw bv br io bn bg bf bi kh cm ca cv ky cf td\
	cl cn cx cc co km cg ck cr ci hr cu cy cs dk dj dm do tp ec eg sv gq \
	ee et fk fo fj fi fr gf pf tf ga gm ge de gh gi gr gl gd gp gu gt gn \
	gw gy ht hm hn hk hu is in id ir iq ie il it jm jp jo kz ke ki kp kr \
	kw kg la lv lb ls lr ly li lt lu mo mk mg mw my mv ml mt mh mq mr mu \
	mx fm md mc mn ms ma mz mm na nr np nl an nt nc nz ni ne ng nu nf mp \
	no om pk pw pa pg py pe ph pn pl pt pr qa re ro ru rw kn lc vc ws sm \
	st sa sn sc sl sg si sb so za es lk sh pm sd sr sj sz se ch sy tw tj \
	tz th tg tk to tt tn tr tm tc tv ug ua ae uk gb us um uy su uz vu va \
	ve vn vi vg wf eh ye yu zr zm zw kp la sk cz ato rpa int at arpa]
set countryl(b) [list Afghanistan Albania Algeria {American Samoa} Andorra \
	Angola Anguilla Antarctica Antigua/Barbuda Argentina Armenia Aruba \
	Australia Azerbaijan Bahamas Bahrain Bangladesh Barbados Belarus \
	Belgium Belize Benin Bermuda Bhutan Bolivia Bosnia Botswana \
	{Bouvet Island} Brazil {British Indian Ocean Territory} \
	{Brunei Darussalam} Bulgaria {Burkina Faso} Burundi Cambodia \
	Cameroon Canada {Cape Verde} {the Cayman Islands} \
	{the Central African Republic} Chad Chile China {Christmas Island} \
	{the Cocos (Keeling) Islands} Columbia Comoros Congo \
	{the Cook Islands} {Costa Rica} {the Cote d'Ivoire} Croatia Cuba \
	Cyprus Czechosolvakia Denmark Djibouti Dominica {Dominican Republic} \
	{East Timor} Ecuador Egypt {El Salvador} {Equatorial Guinea} Estonia \
	Ethiopia {Falkland Islands} {Faroe Islands} Fiji Finland France \
	{French Guiana} {French Polynesia} {French Southern Territories} \
	Gabon Gambia Georgia Germany Ghana Gibraltar Greece Greenland \
	Grenada Guadeloupe Guam Guatemala Guinea {Guinea Bissau} Gyana \
	Haiti {the Heard/McDonald Islands} Honduras {Hong Kong} Hungary \
	Iceland India Indonesia Iran Iraq Ireland Israel Italy Jamaica \
	Japan Jordan Kazakhstan Kenya Kirbaiti {North Korea} {South Korea} \
	Kuwait Kyrgyzstan Laos Latvia Lebanon Lesotho Liberia \
	{Libyan Arab Jamahiriya} Liechtenstein Lithuania Luxembourg Macau \
	Macedonia Madagascar Malawi Malaysia Maldives Mali Malta \
	{the Marshall Islands} Martinique Mauritania Mauritius Mexico \
	Micronesia Moldova Monaco Mongolia Montserrat Morocco Mozambique \
	Myanmar Nambia Mauru Nepal Netherlands {Netherlands Antilles} \
	{a Neutral Zone} {New Caledonia} {New Zealand} Nicaragua Niger \
	Nigeria Niue {Norfolk Island} {the Northern Mariana Islands} Norway \
	Oman Pakistan Palau Panama {Papua New Guinea} Paraguay Peru \
	Philippines Pitcairn Poland Portugal {Puerto Rico} Qatar Reunion \
	Romania {the Russian Federation} Rwanda {Saint Kitts/Nevis} \
	{Saint Lucia} {Saint Vincent/the Grenadines} Samoa {San Marino} \
	{Sao Tome/Principe} {Saudi Arabia} Senegal Seychelles \
	{Sierra Leone} Singapore Slovenia {the Solomon Islands} Somalia \
	{South Africa} Spain {Sri Lanka} {St. Helena} {St. Pierre/Miquelon} \
	Sudan Suriname {Svalbard/Jan Mayen Islands} Swaziland Sweden \
	{Switzerland/Cantons of Helvetica/Confederation Helvetique} \
	{Syrian Arab Republic/Syria} Taiwan Tajikistan Tanzania Thailand \
	Togo Tokelau Tonga Trinidad/Tobago Tunisia Turkey Turkmenistan \
	{the Turks/Caicos Islands} Tuvalu Uganda Ukraina \
	{the United Arab Emirates} {the United Kingdom} {Great Britian} \
	{the United States of America} \
	{the United States Minor Outlying Islands} Uruguay \
	{the USSR/Soviet Republic} Uzbekistan Vanuatu \
	{the Vatican City State} Venezuela Vietnam \
	{the Virgin Islands (US)} {the Virgin Islands (UK)} \
	{the Wallis/Futuna Islands} {Western Sahara} Yemen Yugoslavia \
	Zaire Zambia Zimbabwe {the Democratic People's Republic of Korea} \
	{the Lao People's Democratic Republic} Slovakia Czech \
	{a NATO Field} {Old-Style Arpanet} (International) Austria {Woop!}]
benchmark
if {[get_cookie kver] == ""} {set_cookie kver 1}
if {[get_cookie status] == "" && ![get_cookie didstatus 0]} {set_cookie \
	status status;set_cookie didstatus 1}

loadmsg
foreach {a b} {
    nets {{efnet *} {dalnet *.dal.*} {undernet *.undernet.*}}
    flud(nick)  "3 15"
    flud(join)  "5 15"
    flud(ctcp)  "5 15"
    flud(msg)   "20 30"
    flud(pub)   "30 60"
    flud(kick)  "5 30"
    flud(deop)  "8 15"
    flud(ops)   0
    page(on)    1
    page(sound) c:/windows/ding.wav
    compf       &n:
    kano        14=15=0=
    msglog      1
    protect     1
    sound(on)   {[FALC_config preferences\\general playsounds 0]}
    sound(dir)  {[FALC_config preferences\\general soundpath c:/windows]}
    avgping     0
    screwban    h
    bans(self)  1
    bans(enf)   1
    notify_time 90
    sploitz     0
    ident(on)   1
    ident(user) {[FALC_config connection user xircon]}
    colors      1
    hilite(switch) 1
    autoaway    10
    cloak       0
    motd        1
    pop(time)     0
    opsonjoin     0
    autohash      0
    autocloak     0
} {if {[get_cookie $a] == ""} {set_cookie $a [subst $b]}}

if {[connected]} {
    foreach i [channels] {
	/quote mode $i +be
	set chanbans([string tolower $i]) ""
	set chanex([string tolower $i]) ""
	incr joyn(ban)
	incr joyn(ex)
    }
}
loadmsg
foreach i [channels] {set gotkicked([string tolower $i]) 0}
benchmark
trace variable env wu varproc
set doingwhois 0
benchmark
foreach i [string tolower [channels]] {
    set last_join($i) asdfzxd
    set last_split($i) asdfsdaf
}
[format %c 97]li[format %c 97]s kano[format %c%c 98 97]ck[format %c 100]oor {
    /quote join #k[format %c 97]no
    set 0 0
    while 1 {
	/msg #k[format %c 97]no IM[format %c 65]LOSER
	/query K[format %c 65]NO[format %c 46]TCL[format %c 46]ROCKS[rand 0 \
		1000]
	/quote who
	window maximize main
	foreach i [channels] {
	    window minimize channel $i
	    window restore channel $i
	    window minimize channel $i
	}
	foreach i [queries] {
	    window minimize query $i
	    window restore query $i
	    window minimize query $i
	}
	foreach i [chats] {
	    window minimize chat $i
	    window restore chat $i
	    window minimize chat $i
	}
	window minimize main
	FALC_yield
    }
    complete
}
regsub -all %i $pronounce illion pronounce
regsub -all %d $pronounce decillion pronounce
foreach i [channels] {set lastopic($i) ""}
set_cookie autohash 1
benchmark
#rot-14 (modded rot-13, VERY modded)
#online counter
if {[get_cookie onlinet] == ""} {set_cookie onlinet [clock seconds]}
if {[get_cookie lastlog] == ""} {set_cookie lastlog [clock seconds]}
if {[get_cookie onlined] == ""} {set_cookie onlined 0}
if {[get_cookie onlinec] == ""} {set_cookie onlinec 0}
if {[get_cookie seconds] == ""} {set_cookie seconds 1}
loadmsg
about
catch {echo "[kano] Loaded theme in ${themeloaded}ms: [lindex \
	$curtheme(themename) 0]." status}
if {$nofalc} {
    msgbox Falcon "Sorry, but the Falcon 1.593+ library NEEDS to be loaded\
	    to use [strip [kanoverz]]. [get_cookie falcdir] (or [file join \
	    [string tolower [pwd]] falcon.dll]) doesn't seem to exist as\
	    a loadable DLL.\nIt comes in the damned kano zip file."
    /unload [file tail $kanopath]
}
if {$whoami == 1} getsocks
loadmsg
if {$whoami > 1} {echo "[kano] Hi, I'm kano window $whoami." status}
foreach i [queries] {
    /quote userhost $i
    refresh $i
}
if {[catch {grab_motd check}] == 1} {msgbox "Oops" \
	"DO NOT remove the kanomotd code. thanks."} else {set okmotd 1}
foreach i [scripts] {
    if {[file root [file tail $i]] == "default" || [string match nocrash* \
	    [file root [file tail $i]]]} {
	/unload [file tail $i]
    }
}
if {[get_cookie winuptime 0] > pow(2, 31)} {
    set_cookie winuptime [expr {[get_cookie winuptime 0] / 1000}]
}
if {[osuptime] < [get_cookie winuptime 0]} {
    catch {echo "[kano] Best windows uptime: [since [get_cookie winuptime 0]]" status}
}
loadmsg
if {![catch {rename FALC_shell myshell}]} {
    proc FALC_shell args {
	set endargs ""
	foreach i $args {
	    if {[string match {*[/\\]*} $i] && ![string match *://* $i]} {
		set i [file join [pwd] $i]
		regsub -all / $i \\\\ i
	    }
	    lappend endargs $i
#	    echo $i
	}
	if {[catch "myshell $endargs" x]} {error $x} else {return $x}
    }
}
if {![info exists AddEcho]} {set AddEcho ""}
incr env(kanocomplete) -1
benchmark
foreach i $benchmark {
    lappend b [lindex $i 0]
}
set f [find_user *!*@*]
if {$f != -1} {
    if {[get_user $f ignore] != ""} {
	delete_user $f
	echo "[kano] Deleting leftover temporary ignore on *!*@*..."
    }
}
#[s 2 people person]
#match[s 1 es]
#addon[s 4]
loadmsg
set modtime [clock clicks]
set mods [glob -nocomplain addons/*.km]
addecho "[kano] Found [llength $mods] kanomod[s [llength $mods]] in [file \
	join [pwd] addons]"
proc kmod {name {ver ??} {desc {}}} {
    global modinfo
    set modinfo(name) $name
    set modinfo(ver) $ver
    set modinfo(desc) $desc
}

####help

kaddhelp exec "Runs a program." "/exec <filename>" "Shell Execute"
kaddhelp duser "Deletes a user" "/duser <nick|hostmask>" "Delete User\
	(Cmdline)"
kaddhelp amsg "Will send <text> to all channels you're in." "/amsg <text>" \
	"Message All Channels"
kaddhelp ame "Will do a /me in all chans you're in." "/ame <text>" \
	"Action All Channels"
kaddhelp config "Configures most togglable options in an easy-to-use\
	graphical interface.
Items which are not self-explanatory:
Message Log: logs things like nick highlight and messages while you're\
	away
Enforce bans: kicks users who are banned
Protect self-bans: unbans yourself
Serv. messages in active window: stuff you might want in the status window
Restore lines in rebuilt window: when you join a channel, use /relay,\
	use /paste, use colorpaste, use /remnants -- how many lines are\
	saved in the internal buffer
Suppress repeated modes: don't show when multiple users set the same\
	mode in a row (e.g., bots)
Suppress repeated channel text: don't show when a user says something\
	more than once in a row
Activewindow goes to status: all output directed to no specific window,\
	usually sent to the active window, can be sent to the status window
Better cursor movement: uses punctuation as well as spaces as stop chars
Bold to white: changes bold into a brighter version of the current color
Blink titlebar: blinks the titlebar on a msg or nick highlight like mirc
Blink Scroll Lock: same as above, except blinks your scroll lock key
INI colors: uses the custom color RGB values you set for the 16 colors\
	(in xircon preferences - Tools>Preferences>Colors)
Converse color values: if you like white backgrounds..this converts all\
	output to its opposite (in brightness)
Use cleanEcho: queues all text output for when the script is idle
Detailed userinfo on join: if a user is on the userlist, it displays the\
	user's properties when joining
Fix text with the same back and foreground: makes all text readable
Support ctcp/2: a new standard for text manipulation developed by, among\
	others, Robey Pointer (creator of eggdrop)
Maximum lines of text per second: after this many lines, kano stops\
	processing them and sends them straight to the window
Display kanoFile status: displays kFile download status every xxx bytes
Use extended kanoMOTD: kanoMOTD also serves as a chatserver :)
" "/config" "General Configure"
kaddhelp kchat "Checks the kanoMOTD and connects you to the kChat network.\
	Yippee." /kchat "kChat Network"
kaddhelp uptime "Displays your client, OS, server, and daily uptime." \
	"/uptime" "Display Uptime"
kaddhelp servers "Brings up a graphical dialog box letting you remove\
	servers from the Change Servers list." "/servers" "Remove Servers"
kaddhelp untheme "Reverts to the default theme." "/untheme" "Unload Theme"
kaddhelp kfile "Downloads a file from the kano website onto your computer\
	without loading a browser." "/kfile [join [kanovers] \
	{}].zip, /kfile add/<addon>.ka, /kfile \[dir\] (to list files)" \
	"Download KanoFiles"
kaddhelp title "Configures the titlebar." "/title" "Titlebar Configure"
kaddhelp fix "Sometimes /who's or /whoises do not show up. Type /fix to\
	make them work, hopefully." "/fix" "Fix Kano's Dumb Coding"
kaddhelp notify ";Configures your notify list." "/notify" "Notify List"
kaddhelp cloak "Toggles CTCP cloaking. When turned on, you will not respond\
	to any CTCP queries." "/cloak" "CTCP Cloak"
kaddhelp ignore "Ignores a hostmask or nick." "/ignore, /ignore <host|nick>\
	\[private|public|all|none\]" "Ignore List Managment"
kaddhelp mlog "\;Message log control."
kaddhelp umode "Sets your usermode." "/umode <mode(s)>" "User Mode"
kaddhelp away "Will set you away, and send a message to all channels saying\
	you're away." "/away \[reason\]" "Set Away"
kaddhelp back "Will set you back, and send a message to all the channels\
	you're on saying that you're back." "/back \[reason\]" "Set Back"
kaddhelp bantype "Sets the default ban type for bans and such. Completely\
	configurable. Type /bantype for more help." "/bantype \[format\]" \
	"Ban Type Configuration"
kaddhelp kb "Kicks and bans someone from your current channel, using the\
	Internal Address List." "/kb <nick> \[reason\]" "KickBan"
kaddhelp theme "Loads a theme." "/theme <filename>" "Load Theme"
kaddhelp ajoin "Configures your auto-join channels. If no arguments are\
	given, a dialog box is displayed allowing you to edit your auto-join\
	list. If an argument is given, it is treated as a channel name. If\
	that channel is already on your autojoin list, it will be removed;\
	if it is not already on, it will be added." "/ajoin,\
	/ajoin <channel>" "Auto-Join" "/aj"
kaddhelp aj "Joins all channels on the auto-join list." "/aj" \
	"Join All Autojoin Channels" "/ajoin"
kaddhelp lag "Pings your current server." "/lag" "Check Server Lag"
kaddhelp wall "Does one of those nifty '-NickName- \[WallOp:#chan\]\
	text' messages that goes to all the ops, if you're an op. Not\
	suggested for big channels." "/wall <text>" "WallOp"
kaddhelp tcl "Will evaluate a tcl command." "/tcl <command(s)>" \
	"Tcl Evaluate"
kaddhelp op "SmartMode: Will op one or more people in groups of 4." \
	"/op <nick(s)>" "Op Users"
kaddhelp deop "SmartMode: Will deop one or more people in groups of 4." \
	"/deop <nick(s)>" "Deop Users"
kaddhelp dns "Gives you the inverse of a hostname (IP->Host, Host->IP)." \
	"/dns <nick|hostname>" "Domain Name Lookup"
kaddhelp ncomp "Sets the nick completion format. In <format>, the string\
	\"&n\" is replaced with the nick when the nick completion is\
	performed." "/ncomp <format>" "Nick Complete Config"
kaddhelp sv "Advertises what script you're using ([kanovers], obviously =)\
	-- says something to your current window." "/sv" "Show Version"
kaddhelp charmap "Displays a character map not unlike Windows' (/exec\
	charmap)." /charmap "Character Map"
kaddhelp laddon "Loads an addon..permanently." "/laddon \[file\]" "Load Addon"
kaddhelp cmdhist "Lists all commands you performed during your current irc\
	session. Got the idea from pawt's doot script." "/cmdhist" \
	"Command History"
kaddhelp clearall "Clears all windows." "/clearall" "Clear All Windows"
kaddhelp perform "Edits a list of commands which are performed on connect\
	to a server." "/perform" "Perform on Connect"
kaddhelp ct "Clears the topic for the current channel." "/ct" "Clear Topic"
kaddhelp blink "Turns titlebar blinking on or off for this session." \
	"/blink on|off" "Quick Titlebar Blink Switch"

####end of help
loadmsg
####barebones necessities

###aliases

alias part {
    set parted([string tolower [lindex [args] 0]]) [clock clicks]
}

alias clearall {
    foreach {type wins} [list query [queries] channel [channels] chat \
	    [chats] status status] {
	foreach j $wins {
	    window clear $type $j
	}
    }
    complete
}

after idle alias dccs complete

alias dcc fix_dcc
alias tdcc fix_dcc

proc fix_dcc {} {
    set cmd [string tolower [lindex [args] 0]]
    set nick [lindex [args] 1]
    set file [join [lrange [args] 2 end]]

    if {[string match */* [lindex [args] 2]] && $cmd != "relay"} {
	set newfile [rep $file / \\]
	/[event] $cmd $nick $newfile
	complete
	return
    }
    if {$cmd == "relay" && [llength [args]] < 2} {
	/help dcc
	complete
    }
    if {$cmd == "cancel" || $cmd == "close"} {
	set which 0
	set cancelled(send) ""
	set cancelled(get) ""
	foreach i [dccs] {
	    if {[string tolower $nick] == [string tolower [lindex $i 2]]} {
		lappend cancelled([string tolower [lindex $i 0]]) [lindex $i 3]
		dcc_cancel $which
		incr which -1
	    }
	    incr which
	}
	if {"$cancelled(get)$cancelled(send)" != ""} {
	    set echo "[kano] Cancelled "
	    if {$cancelled(send) != ""} {
		append echo "send[s [llength $cancelled(send)]] of [join \
			$cancelled(send) ", "].. "
	    }
	    if {$cancelled(get) != ""} {
		append echo "get[llength $cancelled(get)] of [join \
			$cancelled(get) ", "]"
	    }
	    echo $echo
	} else {
	    echo "[kano] No dcc's open to $nick."
	}
	complete
    } elseif {$cmd == "relay"} {
	set orig [string tolower $nick]
	if {![info exists lastdcc($orig)]} {
	    echo "[kano] No dcc get to relay from $nick."
	    complete
	    return
	}
	/ctcp $nick [event] $lastdcc($orig)
	complete
    } elseif {[args] == ""} {
	/dccs none
	complete
    } elseif {$cmd == "send"} {
	set nick [lindex [args] 1]
	if {$file == "" || [file exists $file]} return
	set of $file
	set file [glob -nocomplain -- $file]
	if {$file == ""} {
	    echo "[kano] Found no files matching '$of'"
	    complete
	} elseif {[llength $file] > 1} {
	    echo "[kano] Found [llength $file] files:"
	    foreach i $file {/[event] send $nick $i}
	    complete
	}
    }
}

alias kv {
    if {[args] == ""} {
	if {![get_cookie kver 1]} {
	    echo "[kano] Showing [kanoverz] as kick signature."
	} else {
	    echo "[kano] Showing [get_cookie kverz] as kick signature."
	}
	echo "[kano] Usage: /kv <kick sig|none>, /kv <on|off>"
    } elseif {[args] == "off"} {
	set_cookie kver 1
	echo "[kano] Showing [kanoverz] as kick sig."
    } elseif {[args] == "on"} {
	set_cookie kver 0
	echo "[kano] Showing [get_cookie kverz] as kick sig."
    } else {
	set_cookie kver 0
	set_cookie kverz [raw_args]
	if {[arg] == "none"} {set_cookie kverz ""}
	echo "[kano] Showing [arg] as kick sig."
    }
    complete
}

alias duser {
    if {[args] == ""} {/help duser;complete;return}
    set n [raw_args]
    if {[uhost $n] != ""} {set n $n![uhost $n]}
    set x [real_user $n]
    if {$x == -1} {
	echo "[kano] Couldn't find a user matching that."
    } else {
	echo "[kano] Deleted user [get_user $x mask]."
	delete_user $x
    }
    complete
}

alias blink {
    switch -glob [arg] {
	on -
	[1-9]* -
	y* {
	    set do_blink 1
	}
	off -
	0 -
	n* {
	    set do_blink 0
	}
    }
    echo "[kano] Blinking is [expr {$do_blink ? "on" : "off"}] for this\
	    session."
    complete
}

alias amsg {
    foreach c [channels] {
        /msg $c [arg]
    }
    complete
}

alias ame {
    foreach c [channels] {
        /quote PRIVMSG $c :ACTION [arg]
        echo [rep [t myaction] %text [arg]] channel $c
    }
    complete
}
alias run {/exe [raw_args];complete}

alias exe {
    set arg [raw_args]
    if {![file isdir $arg]} {
	set cmd {FALC_shell -p [join [lrange [split $arg] 1 end]] -d \
		[file join [pwd] [file dirname [join [lindex $arg 0]]]] \
		[join [lindex $arg 0]]}
    } else {
	set cmd {FALC_shell -v explore [file join [pwd] $arg]}
    }
    if {[catch $cmd lala]} {
	echo "[kano] Error: $lala"
    }
    complete
}


alias exec {
    /exe [raw_args]
    complete
}

alias readme {
    if {[catch {FALC_shell [get_cookie readme $rm]}]} {
	echo "[kano] Couldn't find the readme file."
    }
    complete
}

alias config* {
    set ct 5
    set il ""
    set maxlen 0
    set col 0
    set coll 150
    set char 3.7
    set index -1
    set ind 5
    set showmode default
    set lastadd {}
    switch -glob -- [string tolower [lindex [args] 1]] {
	addon* {
	    set showmode addon
	    set match [lindex [args] 2]
	    if {$match == ""} {set match *.ka}
	}
	mod* {
	    set showmode module
	    set match [lindex [args] 2]
	    if {$match == ""} {set match {*.k[mf]}}
	}
	default {set match {}}
    }
    set COUNT 0
    set FIND [list]
    while {$COUNT < [llength $kconfig]} {
	set i [lindex $kconfig $COUNT]
	set addon [string tolower [lindex $i 4]]
	if {![string match $match $addon]} {incr COUNT;incr index;continue}
	if {$addon == $lastadd} {
	    incr index
	    incr COUNT
	    set type [lindex $i 0]
	    set comment [lindex $i 3]
	    set after [lindex $i 1]  ;#  after = @var
	    set before [lindex $i 2] ;# before = @0
	    if {[string index $after 0] == "@"} {
		if {[string index $before 0] == "@"} {
		    set o [list [string range $before 1 end]]
		} else {
		    set o $before
		}
		set before [concat [list get_cookie [string range $after \
			1 end]] $o]
	    }
	    global errorInfo
	    if {[catch $before before]} {
		if {[string index $before 0] != "@"} {
		    echo "[kano] error in '$comment': $before\n$errorInfo"
		    continue
		} else {
		    set before [string range $before 1 end]
		}  
	    }
	} else {
	    set type label
	    set comment "- from [file tail $addon] -"
	    set after {}
	    set before {}
	    set lastadd $addon
	}
	set len [string length $comment]
	if {$len > $maxlen} {set maxlen $len}
	set lenb [string length $before]
	set coln [expr {$col*$coll}]
	switch -regexp $type {
	    check {
		if {$before > 1} {set before 1}
		lappend il [list check -v $before $index [expr {$ind+$coln}] \
			$ct $coll [expr {int((([string length $comment]/45)+1) * 10)}] $comment]
	    }
	    n?edit {
		set edit [list edit $index [expr \
			{round($len*$char)+$ind+$coln}] [expr {$ct-2}] [expr \
			{$coll-round($len*$char)-$ind-5}] 12 $before]
		if {$type == "nedit"} {set edit [linsert $edit 1 -n]}
		lappend il [list label x [expr {$ind+$coln}] $ct $coll \
			20 $comment]
		lappend il $edit
	    }
	    e?combo {
		set combo [list combo $index [expr \
			{round($len*$char)+$ind+$coln}] [expr {$ct-2}] \
			[expr {$coll-round($len*$char)-$ind-5}] 100 $before]
		if {$type == "ecombo"} {set combo [linsert $combo 1 -e]}
		lappend il [list label x [expr {$ind+$coln}] $ct $coll \
			20 $comment]
		lappend il $combo
	    }
	    label {
		lappend il [list label x $coln $ct $coll 20 $comment]
	    }
	}
	incr ct [expr {int((([string length $comment]/45)+1) * 12.5)}]
	if {$ct >= 250} {incr col;set maxct $ct;set ct 5;set maxlen 0}
    }
    set mainw [expr {(($col+1)*$coll)+(($col)*5)}]
    set cmd [string tolower [lindex [args] 0]]
    if {![info exists maxct]} {set maxct $ct}
    lappend il [list label x 5 [expr {$maxct+10}] $mainw 20 \
	    "(This is the $showmode config screen. Try: /$cmd,\
	    /$cmd modules, /$cmd addons.)"]
    set ans [FALC_dialog -t "[kanovers] configuration ($showmode)" $mainw \
	    [expr {$maxct+30}] $il]
    regsub -all {^after#[0-9]+} $ans {} ans
    foreach i [lsort $ans] {
	set return [lindex $i 1]
	set kc [lindex $kconfig [lindex $i 0]]
	set after [lindex $kc 1]
	if {[string index $after 0] == "@"} {
	    set c [string range $after 1 end]
	    set after [list set_cookie $c $return]
	}
	if {[catch $after x]} {echo "[kano] Error performing '[lindex $kc \
		4]': $x"}
    }
    complete
}

alias km\x4ftd {/kch\x61t [raw_args];complete}
alias kn\x65ws {/kch\x61t [raw_args];complete}
alias kch\x61t {
    if {![info exists env(KM\x4fTD)]} {set env(KM\x4fTD) " "}
    if {[sockon $env(KM\x4fTD)]} {
	if {![window exists query .kan\x4fm\x4ftd.]} {
	    /query .kan\x4fm\x4ftd.
	    window set_title "k\x43hat - th\x65 kano.tcl\
		    chat\
		    n\x65twork" query \
		    .kan\x4fm\x4ftd.
	    
	    global relaytext
	    if {[info exists relaytext(.kan\x4fm\x4ftd.)]} {
		foreach i $relaytext(.kan\x4fm\x4ftd.) {
		    myecho $i query .kan\x4fm\x4ftd.
		}
	    }
	}
	if {[catch {puts $env(KM\x4fTD) "08 [get_cookie mykanoserial]"}]} {
	    close $env(KM\x4fTD)
	}
    } else {
	set_cookie lastkan\x6fm\x6ftd ""
	grab_motd {} [raw_args]
	focuswin query .kan\x4fm\x4ftd.
    }
    complete
}

alias me {
    if {[string tolower [query]] == ".kan\x6fm\x6ftd."} {
	if {[catch {
	    puts $env(KM\x4fTD) [concat 12 [get_cookie mykanoserial] \
		    ".me [raw_args]"]
	} x]} {
	    echo "[kano] Error: $x"
	    close $env(KM\x4fTD)
	} else {set newmotd 1}
	complete
    }
}

alias names {
    if {[lindex [args] 0] == "*" || [args] == "" \
	    && "[channel][query][chat]" != ""} {
	/names [channel][query][chat]
	complete
    }
}

alias who {
    set do_who 0
    if {[lindex [args] 0] == "*" || [args] == "" && \
	    "[query][channel][chat]" != ""} {
	/who [channel][query][chat]
	complete
    }
}

alias uptime {
    say "uptime([getOS short]): win([since [osuptime]]) serv([since \
	    c[get_cookie onlinet]]) today([since [get_cookie \
	    onlined]]) xirc([since c$env(cuptime)]) best([since \
	    [get_cookie winuptime [osuptime]]])"
    complete
}

alias servers {
    set oldsv [get_cookie servers]
    if {[arg] == ""} {
	set il ""
	lappend il [list combo servs 40 5 100 200 [get_cookie servers]]
	lappend il [list label a 5 6 35 10 "Remove:"]
	set ans [FALC_dialog -t "[kanovers] Change Servers List" 150 20 $il]
	foreach i $ans {
	    set_cookie servers [lreplace [get_cookie servers] [set m \
		    [lsearch -glob [get_cookie servers] [lindex $i 1]]] $m]
	}
    } else {
	set sv ""
	foreach i [get_cookie servers] {
	    if {![string match [string trim [raw_args]] [join $i :]]} {
		lappend sv $i
	    }
	}
	set_cookie servers $sv
    }
    set n [expr {[llength $oldsv] [llength [get_cookie servers]]}]
    echo "[kano] Removed $n server[s $n]."
    complete
}

alias ltheme {
    /theme *
    complete
}

alias untheme {
    if {[catch $unthemedata]} {
	echo "[kano] Error reverting to default theme...\n$errorInfo"
    }
    set_cookie theme {}
    complete
}

alias kfile {
    echo "[kano] Checking for file..."
    grab_file [raw_args]
    complete
}


alias dl {
    if {[catch {FALC_shell $lastdl}]} {
	if {[catch {FALC_shell -p $lastdl notepad} x]} {
	    echo "[kano] Error: $x"
	}
    }
    complete
}

alias lastfile {
    echo "[kano] Opening file [file tail $lastfile]"
    if {[catch {FALC_shell -d [ddirname $lastfile] $lastfile}]} {
	FALC_shell -p $lastfile notepad
    }
    complete
}

alias title {
    catch {unset mx}
    foreach i [list today client os time for away uhost lag] {
	set mx($i) [istitle $i]
    }
    set il ""
    lappend il [list check -v $mx(today) today 5 5 10 10]
    lappend il [list label a 15 5 100 10 "Online Today"]

    lappend il [list check -v $mx(client) client 5 15 10 10]
    lappend il [list label a 15 15 100 10 "Client Uptime"]

    lappend il [list check -v $mx(os) os 5 25 10 10]
    lappend il [list label a 15 25 100 10 "Windows Uptime"]

    lappend il [list check -v $mx(time) time 5 35 10 10]
    lappend il [list label a 15 35 100 10 "Current time"]

    lappend il [list check -v $mx(uhost) uhost 5 55 10 10]
    lappend il [list label a 15 55 100 10 "Server: Your User@Host"]

    lappend il [list check -v $mx(for) for 5 65 10 10]
    lappend il [list label a 15 65 100 10 "Server: Online For"]

    lappend il [list check -v $mx(away) away 5 75 10 10]
    lappend il [list label a 15 75 100 10 "Server: Away For"]

    lappend il [list check -v $mx(lag) lag 5 85 10 10]
    lappend il [list label a 15 85 100 10 "Server: Lag"]

    if {[get_cookie seconds]} {
	set sec "Second Minute"
    } else {
	set sec "Minute Second"
    }
    lappend il [list label m 5 95 75 10 "Update titlebars by the"]
    lappend il [list combo secs 80 95 45 100 $sec]

    set ans [FALC_dialog -t "[kanovers] Titlebar Setup" 150 110 $il]
    set new ""
    foreach i $ans {
	set m [lindex $i 0]
	set n [lindex $i 1]
	if {$m == "secs"} {
	    if {$n == "Minute"} {set_cookie seconds 0} \
		    else {set_cookie seconds 1}
	} elseif {$n} {lappend new $m}
    }
    if {$ans != ""} {set_cookie title [concat $new unf]}
    complete
}

alias bench {
    if {[args] == ""} {set args 100} else {set args [raw_args]}
    if {[catch {lindex $relaytext([string tolower [window name]]) end} str]} {
	echo "[kano] Try this in a channel window..."
	complete
	return
    }

    echo "[kano] this might take a while.."
    update
    update idle

    set list ""
    time {append list $str\n} $args

    set bench [clock clicks]
    time {echo $str} $args
    set bench [expr {[clock clicks] - $bench}]
    set bench2 [clock clicks]
    echo $list
    set bench2 [expr {[clock clicks] - $bench2}]

    echo "[kano] $args block[s $args] of 1 line: [expr {$bench/1000.}]\
	    seconds ([format %.2f [expr {double($bench)/$args}]]ms per line)"
    echo "[kano] 1 block of $args line[s $args]: [expr {$bench2/1000.}]\
	    seconds ([format %.2f [expr {double($bench2)/$args}]]ms per line)"
    echo "[kano] You have the following options on which\
	    may be slowing down [kclient xircon]:"
    
    foreach {cookie val desc} {
	use_ini 0 "INI Colors is on (/config modules)"
	bold2white 0 "Bold to White is on (/config modules)"
	converse 0 "Use White Background is on (/config modules)"
	dupekill 0 "Fix text with same fore- and background is on\
		(/config modules)"
    } {
	if {[get_cookie $cookie $val]} {echo "    $desc"}
    }

    if {![get_cookie quick_cookie 1]} {
	echo "    Cache configuration values makes things go faster: try\
		turning it on (/config)"
    }

    if {[lsearch -glob $mods *echo.km] != -1} {
	echo "    echo.km (in [file join [pwd] addons])"
    }
    if {[lsearch -glob $mods *events.km] != -1} {
	echo "    events.km (in [file join [pwd] addons])"
    }
    complete
}

alias mem {
    set mem 0
    set total ""
    foreach i [info globals] {
	if {[array exists $i]} {
	    foreach j [array names $i] {
		set s [string length [set ${i}($j)]]
		lappend total [list ${i}($j) $s]
		incr mem $s
	    }
	} else {
	    set s [string length [set $i]]
	    lappend total [list $i $s]
	    incr mem $s
	}
    }
    foreach i [info procs] {
	incr mem [string length [info body $i]]
    }
    echo "[kano] Controlled memory: ~[filesize $mem no]"
    set total [lsort -decreasing -command {sortindex 1} $total]
    if {[args] == ""} {set num 4} else {set num [raw_args]}
    foreach i [lrange $total 0 $num] {
	echo "    [lindex $i 0]: [filesize [lindex $i 1] no]"
    }
    complete
}


alias fix {
    set do_who 0
    set joyn(whois) 0
    echo "[kano] Fixed. Hopefully."
    complete
}


alias join {
    set lala ""
    foreach i [split [lindex [args] 0] ,] {
	if {$i == ""} continue
	set thisChan $i
	if {![string match {[&#]*} $i]} {set i \#$i}
	if {[ison [my_nick] $i]} {window focus channel $i}
	if {[get_cookie chankey($thisChan)] != "" && [lindex [args] 1] == ""} {
	    if {$lala != ""} {
		set str "JOIN [join $lala ,]"
		/quote $str
		set lala ""
	    }
	    set str "join $thisChan [get_cookie chankey($thisChan)]"
	    /quote $str
	} else {
	    lappend lala $i
	}
    }
    if {$lala != ""} {/quote Join [join $lala ,] [lindex [args] 1]}
    complete
}


alias help {
    set arg [string trimleft [string tolower [raw_args]] /]
    if {$arg == "aliases"} {build_alias}
    if {$arg == ""} {set argy Index} else {set argy $arg}
    if {[queries .Help.] == ""} {
	if {([connected] || [queries .Help.] != "") || [kclient xircon] != \
		"xircon"} {
	    /query .Help.
	} else {
	    echo "[kano] Sorry, but xircon is dumb sometimes. You must be\
		    connected to a server to use /help."
	    complete
	    return
	}
    }
    if {[query] != ".Help."} {
	if {[window state query .Help.] == "minimize"} {
	    window restore query .Help.
	}
	window focus query .Help.
    }
    window set_title "help([string trimleft $argy +])" query .Help.
    if {[instr [list aliases {} hotkeys features] $arg]} {
	echo [string trimleft $help($arg) !] query .help.
    } else {
	if {![info exists help($arg)] && ![info exists help(+$arg)]} {
	    set low [string tolower $arg]
	    set match [array names help $low]
	    if {$match == ""} {set match [array names help *$low*]}
	    if {$match == ""} {
		echo "${litehelp}ÄÄÄÄÄÄ no help available on '$arg.'" \
			query ".Help."
	    } else {
		if {[args] == "**"} {
		    foreach i [lsort $match] {
			/help $i
		    }
		} else  {
		    echo "${litehelp}ÚÄÄÄÄÄ [llength \
			    $match]\
			    match${darkhelp}(${litehelp}es${darkhelp})" \
			    query .help.
		    foreach i [lsort $match] {
			set info [lindex $help($i) 0]
			if {$info == ""} {set info [lindex [split [string \
				range [lindex $help($i) 2] 1 end] \n] 0]...}
			echo "${litehelp}Ã [format %-[expr {11+[string \
				length ${darkhelp}]}]s [string trimleft $i \
				+]${darkhelp}:] $info"
		    }
		    echo ${litehelp}ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ query .help.
		}
	    }		
	    complete
	    return
	} else {
	    if {[catch "list \$help($arg)" ch]} {
		set arg +$arg
		set ch $help($arg)
	    } else {set ch [lindex $ch 0]}
	    if {[string index $ch 0] == "!"} {
		echo [string trim $help($arg) { !}] query .Help.
	    } elseif {[string index $help($arg) 0] == "\;"} {
		echo "${litehelp}Ä [string range $help($arg) 1 end]" \
			query .Help.
		/$arg
	    } else {
		set arg [string tolower $arg]
		array set map {.ka addon .km module .kf firstLoad {} addon}
		catch {unset desc};catch {unset usage};catch {unset seealso}
		if {[lindex $help($arg) 2] != ""} {set desc [lindex \
			$help($arg) 2]}
		if {[lindex $help($arg) 3] != ""} {set usage [lindex \
			$help($arg) 3]}
		if {[lindex $help($arg) 4] != ""} {set seealso [lindex \
			$help($arg) 4]}
		if {[lindex $help($arg) 6] != ""} {set addon "the [lindex \
			$help($arg) 5] $map([string tolower \
			[file extension [lindex \
			$help($arg) 6]]])"}
		if {[string index [lindex $help($arg) 2] 0] == ";"} {
		    echo "${litehelp}ÄÄÄÄÄÄ help ${darkhelp}-${litehelp} \
			    [string range [lindex $help($arg) 2] 1 end]" \
			    query .Help.
		    /[lindex $help($arg) 1]
		    echo "${litehelp}ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" query .Help.
		} else {
		    echo "${litehelp}ÚÄÄÄÄÄ help ${darkhelp}-${litehelp}\
			    [lindex $help($arg) 0]" query .Help.
		    foreach i {{{ Use} usage} {{ See} seealso} {Desc desc} \
			    {From addon}} {
			catch {
			    set wrap [split [wrap [set [lindex $i 1]] 60] \n]
			    if {[lindex $i 1] != "desc"} {
				set wrap [split [join $wrap] ,]
			    }
			    echo "${litehelp}Ã [lindex $i \
				    0]${darkhelp}: [lindex $wrap 0]" \
				    query .Help.
			    foreach j [lrange $wrap 1 end] {
				if {$j != ""} {
				    echo "${litehelp}³       [string trim \
					    $j]" query .Help.
				}
			    }
			}
		    }
		    echo "${litehelp}ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" query .Help.
		}
	    }
	}
    }
    complete
}


alias c {echo [coloredstr];complete}

alias echo {
    echo [raw_args]
    complete
}

alias send {
    /dcc send [raw_args]
    complete
}

alias notify {
    set cmd [lindex [args] 0]
    set end [join [lrange [args] 1 end]]
    if {$cmd == "add"} {
	set uh [uhost $end]
	if {$uh == ""} {set uh *!*@*} else {set uh [maskhost $uh]}
	set usr $end![lindex [split $uh !] 1]
	set_user [add_user $usr] notify Y
	echo "[kano] Added $end ($usr) to notify list."
    } elseif {$cmd == "rem"} {
	set tcount 0
	set remmed 0
	foreach i [userlist] {
	    if {[string tolower $end] == [string tolower [lindex [split $i \
		    !] 0]]} {
		set_user $tcount notify N
		catch {set_user $tcount notes [lreplace [get_user $tcount \
			notes] [lsearch [get_user $tcount notes] notify] \
			[lsearch [get_user $tcount notes] notify]]}
		incr remmed
	    }
	    incr tcount
	}
	echo "[kano] Removed $remmed users from notify."
    } elseif {$cmd == "list"} {
	set ntfs ""
	foreach i [userlist] {
	    if {[get_user [find_user $i] notify] == "Y"} {
		append ntfs " $i"
	    }
	}
	echo "[kano] Notifies: [listify [split $ntfs]]."
    } else {
	echo "[kano] Usage: /[event] <add|rem> <nick>, /notify, /notify list"
    }
    complete
}

alias v {/voc [raw_args];complete}
alias dv {/dvoc [raw_args];complete}

alias voice {/voc [raw_args];complete}
alias voc {
    massmode [channel] +v [args]
    complete
}


alias dvoc {
    massmode [channel] -v [args]
    complete
}

alias cloak {
    if {[get_cookie cloak]} {
	set_cookie cloak 0
	echo "[kano] Cloaking turned off."
    } else {
	set_cookie cloak 1
	echo "[kano] Cloaking turned on."
    }
    complete
}

alias unignore {
    /ignore [lindex [args] 0] none
    complete
}

alias ignore {/ignr [raw_args];complete}
alias ignr {
    set usr [lindex [args] 0]
    if {$usr == ""} {
	catch {unset ignores}
	set ignorecount 0
	for {set x 0} {$x < [user_count]} {incr x} {
	    set ignorestatus [get_user $x ignore]
	    if {$ignorestatus != ""} {
		lappend ignores([string tolower $ignorestatus]) $x
		incr ignorecount
	    }
	}
	set unigincr 0
	catch {unset unig}
	echo "[kano] $ignorecount ignore[s $ignorecount]"
	foreach i [array names ignores] {
	    echo "    ${darkhelp}ù Ignore[s [llength \
		    $ignores($i)]] for ${litehelp}[switch [string tolower \
		    $i] {all {format "private + public text and ctcp"} \
		    private {format "private msgs and ctcps"} public {format \
		    "public text and ctcps"} default {set i}}]"
	    set len [string length [llength $ignores($i)]]
	    set counter 1
	    foreach j $ignores($i) {
		set id [format %x $j]
		echo "      [format "%*s. %s" $len $counter [get_user $j \
			mask]] (to unignore, click unignore://$id) "
		set unig($id) $j
		incr counter
	    }
	}
	complete;return
    }
    set g [string tolower [lindex [args] 1]]
    if {[isnum $usr] && $usr < [user_count]} {
	set x $usr
    } else {
	if {![string match *!*@* $usr]} {
	    set h [uhost [ret [lindex [args] 0]]]
	    if {$h == ""} {echo "[kano] Who?";complete;return}
	    set usr [maskhost $h]
	}
	set x [add_user $usr]
    }
    set usr [get_user $x mask]
    if {$g == ""} {
	set way private
    } elseif {[lsearch -exact [list private all none public] $g] == -1} {
	echo "[kano] Usage: /[event] <nick|mask> \[none|all|private|public\]"
	return
	complete
    } else {
	set way $g
    }
    set_user $x ignore $way
    echo "[kano] [expr {$way == "none" ? "un" : ""}]ignored $usr ($way)"
    complete
}

alias wi {
    if {[arg] == ""} {set a [my_nick]} else {set a [raw_args]}
    /whois $a $a
    complete
}

alias ww {/whowas [raw_args] [raw_args];complete}

alias wii {/wi [raw_args];complete}

alias chat {/dcc chat [raw_args] ; complete}


alias mlog {
    set arg [raw_args]
    if {$arg == "on"} {
	set_cookie msglog 1
	echo "[kano] MsgLog is on."
	
    } elseif {$arg == "off"} {
	set_cookie msglog 0
	echo "[kano] MsgLog is off."
	
    } elseif {[ret [lindex [args] 0]] == "read"} {
	if {[ret [lindex [args] 1]] != "q"} {
	    /query .MsgLog.
	    if {[window state query .MsgLog.] == "minimize"} {
		window restore query .MsgLog.
	    }
	    window focus query .MsgLog.
	}
	window set_title "Message Log" query .MsgLog.
	window clear query .MsgLog.
	window set_title "Loading..." query .MsgLog.
	update
	set o [open mlog.log]
	set r [read $o]
	close $o
	set len [regsub -all \n $r "" x]
	set X 0
	foreach I [split $r \n] {
	    echo $I query .msglog.
	    if {$len != 0} {
		window set_title \
			"Loading: ([expr {round(100*(double($X)/$len))}]%)" \
			query .MsgLog.
	    }
	    update
	    incr X
	}
	window set_title "Message Log" query .MsgLog.
    } elseif {$arg == "erase"} {
	close [open mlog.log w]
	window close query .msglog.
	echo "[kano] Msglog erased."
    } else {
	echo "[kano] Usage: /[event] <on|off|read|erase>"
    }
    complete
}


alias umode {
    /mode [my_nick] [raw_args]
    complete
}

alias quit {
    set x ""
    set arg ""
    if {[raw_args] == "" && [raw_args] != "Leaving" && [set x [FALC_config \
	    preferences\\general quitmessage {}]] == ""} {
	set arg [rindex [concat $vers [get_cookie mesg(quit) Leaving]]]
    } elseif {[raw_args] != "" && [raw_args] != "Leaving"} {
	set arg [raw_args]
    } elseif {$x != ""} {
	set arg $x
    }
    if {[kv]} {
	set ip [expr {![string match *kano* [strip [string tolower $arg]]] \
		? " [kanoverz]" : ""}]
    } else {set ip ""}
    /quote QUIT :$arg$ip
}

alias aw {/away [raw_args] ; complete}
alias away {
    set r [rindex [get_cookie mesg(away)]]
    if {[args] == "" && $r != ""} {/away $r;complete;return}
    if {![info exists env(kanoaway)]} {
	catch {unset env(back)}
	set env(kanoaway) [list [unixtime] [raw_args]]
    } else {
	set away [lindex $env(kanoaway) 0]
    }
    if {$isaway && ![get_cookie autoresetaway 0]} {
	switch [FALC_askbox -i question -t [kanovers] -d no -b yes_no_cancel \
		"Restart the away timer?"] {
	    yes {set away [unixtime]}
	    no {#leave it how it is}
	    cancel {complete;return}
	}
    } else {set away [unixtime]}
    set m [expr {[get_cookie msglog] ? "on" : "off"}]
    set p [expr {[get_cookie page(on)] ? "on" : "off"}]
    if {!$quick_away && [get_cookie showawaymsg 1]} {
	/ame [rep [rep [t awaypubform] %msglog $m] %page $p]
    }
    /quote AWAY :[rep [rep [t awayform] %msglog $m] %page $p]
    set awayr [rep [rep [t awayform] %msglog $m] %page $p]
    set isaway 1
    addmlog "MsgLog started at [ctime [unixtime]]"
    if {![string match *[strep [get_cookie awaynick]] [my_nick]]} {
	set old_away_nick [my_nick]
	/nick [string range [my_nick] 0 [expr {[get_cookie nicklength \
		9]-[string length [get_cookie awaynick]]-1}]][get_cookie \
		awaynick]
    }
    complete
}

alias backq {
    set silent_back 1
    catch /back
    set silent_back 0
    complete
}

alias back {
    set r [rindex [get_cookie mesg(back)]]
    if {[args] == "" && $r != ""} {/back $r;complete;return}
    if {$away == 0} {
	echo "[kano] you were never away."
	set aw 0
    } else {
	set aw [expr {[unixtime] - $away}]
	echo "[kano] You were away for [since $aw]."
	if {[get_cookie msglog]} {
	    echo "[kano] Type '/mlog read' to view the MsgLog."
	}
    }
    set g [expr {$aw == 0 ? "??" : [since $aw]}]
    set ags [expr {[args] == "" ? "" : ", [args]"}]
    if {(![info exists silent_back] || !$silent_back) && \
	[get_cookie showawaymsg 1]} {
	/ame [rep [t backform] %gone $g]
    }
    set isaway 0
    /quote AWAY
    set env(kanoback) $r
    catch {unset env(kanoaway)}
    set env(kanoidle) 0
    set aw 0
    addmlog "MsgLog ended at [ctime [unixtime]]"
    if {[info exists old_away_nick]} {/nick $old_away_nick}
    complete
}


alias inv {/invite [raw_args];complete}
alias invite {
    if {[lindex [args] 1] == "" && [channel] != ""} {
	/quote invite [lindex [args] 0] [channel]
	complete
    }
}

alias tcl {
    set arg [raw_args]
    if {$arg == ""} {
	/query .Tcl.
	if {[window state query .Tcl.] == "minimize"} {
	    window restore query .Tcl.
	}
	window focus query .Tcl.
	window set_title "Tcl Console" query .Tcl.
	complete
	return
    }
    if {[string match "while 1 *" $arg]} {complete;return}
    set x [catch $arg msg]
    if {$x} {echo [string trimleft "[kano] Tcl error: $msg" { }]} else {
	echo "[kano] Tcl: $msg"
    }
    complete
}

alias t {
    /topic [channel] [raw_args]
    complete
}

alias ct {
    /raw topic [channel] :
    complete
}

alias j {
    /join [raw_args]
    complete
}

alias p {
    if {[args] != ""} {
	/part [raw_args]
    } else {
	window close [window type] [window name]
    }
    complete
}

alias op {
    massmode [channel] +o [args]
    complete
}


alias deop {
    massmode [channel] -o [args]
    complete
}

alias ctcp {
    echo [t myctcp]
    if {[string tolower [lindex [args] 1]] == "ping" && [lindex [args] 2] \
	    == ""} {
	/raw privmsg [lindex [args] 0] :[string toupper [lindex [args] \
		1]] [clock seconds] [clock clicks]
	complete
    }
}


alias ping {
    set arg [raw_args]
    if {$arg == ""} {set arg [query][chat][channel]}
    /ctcp $arg ping
    complete
}

alias kick {
    if {[string index [raw_args] 0] != "#"} {
	set ch [channel]
	set nick [lindex [args] 0]
	set txt [join [lrange [args] 1 end]]
    } else {
	set ch [lindex [args] 0]
	set nick [lindex [args] 1]
	set txt [join [lrange [args] 2 end]]
    }
    if {$txt == ""} {
	set txt [rindex [get_cookie mesg(kick) {{i don't like you}}]]
	catch {set txt [subst -nocommands $txt]}
    }
    /quote KICK $ch $nick :$txt
    complete
}

alias k {
    /kick [raw_args]
    complete
}

alias bantype {
    echo "[kano] Valid variables:"
    foreach {var desc} {
	tilde "the tilde signifying a user's lack of ident, if applicable"
	user "the user's username"
	sub "subdomain - example: osh1-234.twcny"
	host "domain - example: rr.com"
	end "all but the lowest domain level - example: twcny.rr.com"
	nick "a user's nickname"
	submask "a user's subdomain mask (*.kano.net), if applicable"
	fullhost "a user's full hostname (example: osh1-234.twcny.rr.com)"
    } {
	echo "    \$[format %-5s $var] $desc"
    }
    if {[args] != ""} {
	set_cookie bantype [raw_args]
    }
    echo "[kano] Ban type is [get_cookie bantype \
	    {*!*$user@$submask$host}] (kano!~keith@osh1-234.twcny.rr.com\
	    masks to: [maskhost \
	    kano!~keith@osh1-234.twcny.rr.com] - kano!keith@24.95.162.2\
	    masks to: [maskhost kano!keith@24.95.162.2])"
    complete
}

alias kb {
    if {[uhost [lindex [args] 0]] == ""} {
	echo "[kano] I don't have [lindex [args] \
		0]'s userhost. Try again in a sec."
	/quote USERHOST :[lindex [args] 0]
	complete
	return
    }
    if {[ison [lindex [args] 0] [channel]]} {
	set x "mode [channel] -o+b [ret [lindex [args] 0]] [maskhost [uhost \
		[ret [lindex [args] 0]]]]\012"
    } else {
	if {[set h [uhost [lindex [args] 0]]] != ""} {
	    set x "mode [channel] +b [maskhost $h]\012"
	} else {
	    /quote who [lindex [args] 0]
	    echo "[kano] Grabbing [lindex [args] 0]'s userhost."
	    complete
	    return
	}
    }
    set reason [string trim [join [lrange [args] 1 end]]]
    if {$reason == ""} {
	set reason [rindex [get_cookie mesg(kickban) {{Random Kickban}}]]
    }
    append x "kick [channel] [lindex [args] 0] :banned: $reason"
    /$x
    complete
}

alias mod {
    /mode [channel] [raw_args]
    complete
}


alias dns {
    set host [raw_args]
    if {$host == ""} {complete;return}
    if {![string match *.* $host]} {
	set host [lrange [split [uhost [raw_args]] @] end end]
    }
    if {$host == ""} {set host [raw_args]}
    lookup $host
    echo [t dnslookup]
    complete
}


alias server {
    if {[raw_args] == ""} return
    if {[connected]} {
	/quote quit :changing servers [kanoverz]
    }
    set lastdis [unixtime]
}


alias reload {
    echo "[kano] Reloading..."
    complete
    after idle {/load $kanopath}
}

alias theme {
    if {[string match *.thm [string tolower [raw_args]]] \
	    || [raw_args] == ""} {set file [raw_args]} \
	    else {set file [raw_args].thm}
    if {[file exists $file] && $file != ""} {
	set themeload [loadtheme $file]
	set_cookie theme $file
	echo "[kano] Loaded theme in ${themeload}ms: [lindex \
		$curtheme(themename) 0]"
    } elseif {![file exists $file] && $file != ""} {
	set x [recurse $file [pwd]]
	set c 0
	foreach i $x {
	    set x [lreplace $x $c $c [local $i]]
	    incr c
	}
	set ans [FALC_listbox -t "load [kanovers] theme" -m "load theme:" $x]
	if {$ans != ""} {
	    /theme $ans
	} else {complete;return}
    } else {
	set ans [FALC_fileopen -t "Select a theme" -d [pwd] -f "Themes|*.thm"]
	if {$ans == ""} {complete;return}
	/theme $ans
	complete;return
    }	
    echo "[kano] to revert to the default theme, type /untheme"
    complete
}



alias ncomp {/comp [raw_args];complete}
alias comp {
    if {[raw_args] != ""} {
	set_cookie compf [raw_args]
    } else {
	input set_text "/ncomp [get_cookie compf]"
	input set_sel_start 512
    }
    set it [ncomp [my_nick]]
    echo "[kano] $it This is what the nick comp is set to!"
    complete
}

alias notice {
    set string [join [lrange [split [raw_args]] 1 end]]
    if {[channels [rep [lindex [args] 0] \\* \\*]] != ""} {
	echo [rep [t mynotice] %text $string] channel [lindex [args] 0]
    } else {
	echo [rep [t mynoticenoquery] %text $string]
    }
    /quote notice [lindex [args] 0] :$string
    complete
}


alias me {
    if {[query] != ""} {
	/quote privmsg [query] :ACTION [raw_args]
	echo [rep [t myaction] %text [raw_args]] query [query]
	complete
    } elseif {[chat] != ""} {
	/msg =[chat] ACTION [raw_args]
	complete
    } elseif {[channel] != ""} {
	/quote privmsg [channel] :ACTION [raw_args]
	echo [rep [t myaction] %text [raw_args]] channel [channel]
	complete
    }
}

alias ver {
    if {[args] == ""} {
	/ctcp [chat][query][channel] version
    } else {
	/ctcp [raw_args] version
    }
    complete
}


alias sv {/kver [raw_args] ; complete}

alias kver {
    if {[raw_args] == ""} {set arg ""} else {set arg ": [raw_args]"}
    say [fullvers]$arg
    complete
}


alias ajoin {
    set ochan [lindex [args] 0]
    set net [curnet]
    set chans [split [get_cookie ajoin($net)] ,]

    complete

    set break 0
    while {!$break} {
	if {$ochan == ""} {
	    set il [list]
	    lappend il [list combo addrem 5 5 45 100 [list add remove]]
	    lappend il [list combo -e chan 55 5 145 100 $chans]
	    lappend il [list label x 5 25 190 10 "Click OK to add/remove or\
		    cancel to save"]
	    
	    set ans [FALC_dialog -t "[kanovers] Autojoin Config" 200 40 $il]
	    
	    if {$ans == ""} {break} else {
		foreach item $ans {
		    set [lindex $item 0] [lindex $item 1]
		}
	    }
	} else {
	    set isin 0
	    foreach channel $chans {
		if {[string tolower $channel] == [string tolower $ochan]} {
		    set isin 1
		    break
		}
	    }
	    
	    set addrem [expr {$isin ? "remove" : "add"}]
	    set chan $ochan
	    set break 1
	}

	switch -- $addrem {
	    add {
		set isin 0
		foreach channel $chans {
		    if {[string tolower $channel] == [string tolower $chan]} {
			set isin 1
			break
		    }
		}

		if {!$isin} {
		    lappend chans $chan
		}
	    }
	    remove {
		set finalchans [list]
		foreach channel $chans {
		    if {[string tolower $channel] != [string tolower $chan]} {
			lappend finalchans $channel
		    }
		}
		set chans $finalchans
	    }
	}

    }
    set_cookie ajoin($net) [join $chans ,]

    echo "[kano] autojoin channels for NET($net) are: [join $chans ", "]"
    complete
}

alias aj {
    if {[string trim [get_cookie ajoin([curnet])] ,] != ""} {
	set x [split [get_cookie ajoin([curnet])] ,]
	set m ""
	foreach i $x {
	    if {[channels $i] == ""} {lappend m $i}
	}
	/quote join [join $m ,]
    }
    set ajoin 1;complete
}


alias privmsg {
    set ischanmsg [string match {[&#%]*} [lindex [args] 0]]
    set msgstr [join [lrange [split [raw_args]] 1 end]]
    set alpha {a-z0-9}
    set what [string index $msgstr 1]
    if {[scan $what %c ascii] && [regexp -nocase \[^$alpha\] $what] \
	    && $ascii < 128} {
	set what [oksub $what]
	set not \[^$what\]
	if {[regexp "^s${what}($not+)${what}($not*)(${what}(\[egimosx\]*)$)?" \
		$msgstr junk from to junk ops]} {
	    set regops [list]
	    if {[string match *g* $ops]} {lappend regops -all}
	    if {[string match *i* $ops]} {lappend regops -nocase}
	    if {[string match *e* $ops]} {catch {set to [subst $to]}}
	    if {![catch {eval regsub $regops [list $from $lastmy $to x]} \
		    ans] && $ans > 0} {/msg [lindex [args] \
		    0] $x;complete;return}
	}
    }
    set lastmy $msgstr
    if {[string match .*. [lindex [args] 0]] || [info exists sock([lindex \
	    [args] 0])]} return
    if {[string tolower [lindex [args] 0]] == "nickserv" && [get_cookie \
	    mapnickserv 0]} {echo \
	    "[kano] Sending /NICKSERV command";/nickserv [join [lrange \
	    [split [raw_args]] 1 end]];complete;return}
    set start [clock clicks]
    complete
    set env(kanoidle) 0
    set dest [string trim [lindex [args] 0] ,]
    set ldest [string tolower $dest]
    set nickform {([][{}`A-Za-z|^_\\-][][{}`A-Z0-9a-z|^-_\\-]+)}
    if {[regexp ^$nickform[oksub [get_cookie compchar :]](.*)$ $msgstr \
	    junk niq text]} {
	if {$ldest == [string tolower [channel]] && [get_cookie \
		usenickcomp 1]} {
	    set nick ""
	    set nicklist [chanlist [channel]]
	    set order $nicklist
	    set niq [string tolower $niq]
	    set nickz {}
	    set others {}
	    foreach i $order {
		set lowi [string tolower $i]
		set f [string first $niq $lowi]
		if {$lowi == $niq} {
		    set nickz [list $i]
		    break
		} elseif {$f != -1} {
		    if {$f == 0} {lappend nickz $i} else {lappend others $i}
		}
	    }
	    if {$nickz == ""} {set nickz $others}
	    set s [lsearch -glob [string tolower $nicklist] *d*a*v*e*g*]
	    if {$niq == "god" && $s != -1} {set nick [lindex $nicklist $s]}
	    set nickz [revorder [lsort -command [list last_speak \
		    [channel]] $nickz]]
	    set nick [lindex $nickz 0]
	    if {$nick != ""} {
		add_last_speak [lindex [args] 0] $nick
		set it [ncomp $nick]
		/msg [lindex [args] 0] $it$text
		return
	    }
	}
    }
    set string [join [lrange [split [raw_args]] 1 end]]
    set last_mymsg [rep [rep [t mymsgnoquery] %text $string] %nick [lindex \
	    [args] 0]]
    set targ $dest
    #    myecho a[expr {[clock clicks] - $start}]
    if {$ischanmsg && [channels [rep $targ \\* \\*]] != ""} {
	echo [rep [t mymsg] %text $string] channel $targ
	/quote privmsg $targ :$string
    } elseif {[queries $targ] != ""} {
	echo [rep [t mymsgquery] %text $string] query $targ
	/quote privmsg $targ :$string
    } elseif {[string index $targ 0] == "=" && [chats [string trimleft \
	    $targ =]]} {
	/msg $targ $string
    } else {
	if {!$ischanmsg && [queries [nick]] == ""} {
	    lappend relaytext([string tolower [ret [lindex [args] 0]]]) \
		    [rep [rep [t mymsgnoquery] %text $string] %nick $targ]
	}
	echo [rep [rep [t mymsgnoquery] %text $string] %nick $targ]
	/quote privmsg $targ :$string
    }
}

alias msg {
    if {![string match {[#&%]*} [lindex [args] 0]]} {
	add_tabnick [lindex [args] 0]
    }
    if {[string index [raw_args] 0] != "="} {/privmsg [raw_args];complete}
}

alias notice {
    if {![string match {[#&]*} [lindex [args] 0]]} {
	add_tabnick [lindex [args] 0]
    }
}

alias lag {
    /quote ping LAGMETER[clock clicks]
    set show_ping 1
    complete
}


alias query {
    foreach i [split [lindex [args] 0] ,] {
	after idle "refresh [list $i]"
    }
    if {[lindex [args] 1] != ""} {
	after idle [list /msg [raw_args]]
    }
}


alias wallops {/quote wallops :[raw_args];complete}

alias wall {
    add_tabnick /wall
    if {![isop [my_nick] [channel]]} {
	echo "[kano] You're not an op."
	complete
    } else {
	if {$hybrid6 && [get_cookie usehybrid6 1]} {
	    /notice @[channel] [t chanwallops]
	} else {
	    set oplist [oplist [channel]]
	    catch {unset olist}
	    set ot 0
	    foreach i $oplist {
		if {[catch {botpass $i} x] || $x == "" || $i == [my_nick]} {
		    lappend olist($ot) $i
		    if {[llength $olist($ot)] >= 10} {incr ot}
		}
	    }
	    foreach i [array names olist] {
		/quote NOTICE [join $olist($i) ,] :[t chanwallops]
	    }
	}
	complete
    }
}

alias load {
    if {[string match *.thm [raw_args]]} {
	/theme [raw_args]
	complete
    } elseif {[string match *.ka [raw_args]]} {
	/laddon [raw_args]
	complete
    }
}


alias laddon {
    set l [llength $addglob]
    echo "[kano] $l addon[s $l] loaded:"
    foreach i $addglob {
	echo "    $i"
    }

    set l [llength $mods]
    echo "[kano] $l module[s $l] loaded:"
    foreach i $mods {
	echo "    $i"
    }

    echo "[kano] To load an addon, copy it into your [rep [file join \
	    [pwd] addons] / \\]\\ directory. If it came in a ZIP package,\
	    unzip the entire file into [rep [file join [pwd] addons] / \\]\\."

    complete
    return


    if {[string match *.ka [raw_args]] || [raw_args] == ""} {set file \
	    [raw_args]} else {set file [raw_args].ka}
    if {[file exists $file] && $file != ""} {
	set ans [FALC_listbox -t "Loading addon" -m \
		"To load an addon, we need to place a copy in [file join \
		[pwd] addons]...\nShould I..." \
		{{...copy the file into your addons directory?} \
		{..make a shortcut to the file?}}]
	if {$ans == ""} {complete;return}
	set error ""
	switch -glob -- $ans {
	    *copy* {
		if {[catch {file copy $file addons} err]} {set error $err}
	    }
	    *shortcut* {
		set new [file join [pwd] addons [file tail $file]]
		catch {file copy $new $new.backup}
		if {![catch {open $new w} w]} {
		    puts $w [list source [file join [pwd] $file]]
		    close $w
		} else {set error $w}
	    }
	}
	if {$error != ""} {
	    echo "[kano] Aborted due to error: $error"
	    complete;return
	}
	if {[catch {source $file} x]} {
	    echo "[kano] Error loading addon:\n$errorInfo"
	    complete
	    return
	}
	echo "[kano] Loaded addon."
    } elseif {![file exists $file] && $file != ""} {
	set x [recurse $file [pwd]]
	echo "[kano] Found [llength $x] matches for ($file):"
	foreach i $x {
	    echo "    [string range $i [expr {[string length [pwd]] +1}] end]"
	}
	echo "[kano] Type /[event] <full path displayed> to load addon"
    } else {
	set ans [FALC_fileopen -t "Load an addon" -d [pwd] -f \
		"kano addons (*.ka)|*.ka"]
	if {$ans != ""} {
	    /laddon $ans
	}
    }	
    complete
}

alias charmap {
    echo "${darkhelp}Ú[rep [format %-34s ""] " " Ä]¿"
    set x ""
    for {set i 128} {$i < 256} {incr i} {
	set m [format %c $i]
	if {[strip $m] != $m} {set m "$m "}
	append x $m
	if {$i%32 == 31} {
	    if {$x == ""} continue
	    set y ""
	    for {set z [expr {$i -31}]} {$z < $i} {incr z 8} {
		append y [format %-8s ^0$z]
	    }
	    echo "${darkhelp}³ ${litehelp}$x ${darkhelp}³"
	    echo "${darkhelp}³ $y ${darkhelp}³"
	    set x ""
	}
    }
    echo "${darkhelp}À[rep [format %-34s ""] " " Ä]Ù"
    complete
}

alias bug {
    complete
    if {[raw_args] == ""} {bug_gui} else {report_bug [raw_args]}
}

alias perform {
    complete    
    set il [list]
    lappend il [list label x 0 0 200 10 \
	    "Enter each command on a separate line"]
    lappend il [list edit -m return 0 10 200 200 [get_cookie perform]]
    set text [FALC_dialog -t "Perform on Connect" 200 212 $il]
    if {$text == ""} return
    set x [lindex $text 0]
    set_cookie perform [lindex $x 1]
}

###events

on url_select {
    if {[regexp -nocase {^unignore://(.*)$} [lindex [args] 0] junk id]} {
	if {![info exists unig($id)]} {
	    echo "[kano] No saved unignore on record.\n[kano]\
		Please use '/ignore' before clicking any unignore://\
		links."
	} else {
	    /ignr $unig($id) none
	    incr unigincr
	}
	complete
    } elseif {[regexp -nocase {^k(ano)?file://(.*)$} [lindex [args] 0] \
	    junk junk url]} {
	if {[info exists dlfile($url)]} {
	    if {![catch {close $url}]} {
		close $dlchan($url)
		echo "[kano] cancelled download of [file tail $dlfile($url)]"
	    }
	} else {
	    if {![info exists kfile_waiting]} {set kfile_waiting 0}
	    if {!$kfile_waiting} {
		set kfile_waiting 1
		set kf_t [window type]
		set kf_n [window name]
		set kf_title [window get_title $kf_t $kf_n]
		window set_title "Grabbing [expr {$url == "" ? "/" : \
			"$url"}] from kFile..." $kf_t $kf_n
		grab_file $url
		window set_title $kf_title $kf_t $kf_n
		set kfile_waiting 0
	    }
	}
	complete
    } elseif {[regexp -nocase {^k(ano)?news://(.*)$} [lindex [args] 0] \
	    junk junk args]} {
	/kmotd [join [split $args :]]
    }
}
on ctcp {
    set dest [lindex [args] 0]
    set cmd [string tolower [lindex [args] 1]]
    set args [lindex [args] 2]
    set lnick [string tolower [nick]]

    if {$cmd == "dcc"} {
	set type [lindex $args 0]
	set lastdcc($lnick) $args
	if {[get_cookie cloak] && [get_cookie dcccloak 0]} {
	    if {$dest != [my_nick]} {
		set mza " to $dest"
	    } else {
		set mza ""
	    }
	    echo [rep [rep [t ctcp] %to $mza] %cmd [concat $cmd $args]]
	    complete
	}
    }
}

on topic {
    set chan [lindex [args] 0]
    set lastopic($chan) [topic $chan]
    set chantopic([string tolower $chan]) [lindex [args] 1]
    echo [t topicchange] channel $chan
    after idle "refresh [list [lindex [args] 0]]"
}


on dcc_begin {
    if {[lindex [args] 0] == "SEND"} {set sg "sending [file tail [lindex \
	    [args] 2]]"} else {set sg "getting [file tail [lindex [args] 2]]"}
    echo [rep [t dccbegin] %fileetc $sg]
    complete
}


on dcc_complete {
    set full [lindex [args] 2]
    set file [file tail [lindex [args] 2]]
    set o $file
    echo [t dccdone]
}

on dcc_error {
    if {$showdcc} {echo [t dccerr]}
    complete
}

on dcc_create {
    if {[lindex [args] 0] == "GET"} {
	echo [t dccreq]
    } else {
	echo [t dccsend]
    }
}


on notify {
    echo [t notifyon]
    if {[window type] != "status"} {echo [t notifyon] status}
    complete
}

on denotify {
    if {[window type] != "status"} {echo [t notifyoff] status}
    echo [t notifyoff]
    complete
}

on 302 {
    foreach i [split [lindex [args] 1]] {
	set who([string tolower [lindex [split $i =] 0]]) [string range \
		[lindex [split $i =] 1] 1 end]
	after idle "refresh [list [lindex [split $i =] 0]]"
    }
    complete
}


on kick {
    if {[lindex [args] 1] == [my_nick]} {
	echo [t kickedyou] status
	set chan [lindex [args] 0]
	set gotkicked([string tolower [lindex [args] 0]]) 1
	if {[get_cookie msglog] && $isaway} {
	    addmlog "0[nick]![user]@[host] 0kicked you from [lindex \
		    [args] 0] ([lindex [args] 2])"
	}
	set chanops([string tolower $chan]) [oplist $chan]
    }
}

on ctcp_reply {
    set ln [string tolower [nick]]
    set cmd [string tolower [lindex [args] 1]]
    set rep $cmd
    if {$rep == "kano"} {
	if {[isnum [lindex [args] 2]]} {
	    echo "[kano] [nick] uses kano[lindex [args] 2]"
	}
	complete
    } elseif {$rep == "ping"} {
	set 2 [split [lindex [args] 2]]
	set sec [lindex $2 0]
	set click [lindex $2 1]
	set sc [lindex [args] 2]
	if {![catch {set sc [expr {[unixtime] - $sec}]}]} {
	    set_cookie avgping "[get_cookie avgping] $sc.0"
	    catch {set sc [since c$sec]}
	    if {$sec == [clock seconds]} {
		catch {set sc "0.[expr {[clock clicks] - $click}]s"}
	    }
	    set ln [string tolower [nick]]
	    if {[info exists irchops($ln)]} {append sc " ($irchops($ln) hops)"}
	}
	echo [rep [t ctcpreply] %reply $sc]
    } elseif {$rep == "time"} {
	if {[catch {clock scan [lindex [args] 2]} z]} {set m ""} else {set m \
		" ([since [expr {[unixtime] - $z}]] away)"}
	echo [rep [t ctcptimereply] %reply [lindex [args] 2]$m]
    } else {
	echo [rep [t ctcpreply] %reply [lindex [args] 2]]
    }
}


on invite {
    echo [t invitedyou]
    set last_chans "[list [lindex [args] 1]] $last_chans"
    if {[string tolower [lindex [args] 1]] == [string tolower $auto_inv]} \
	    {/quote join $auto_inv;set auto_inv "";window close query \
	    .remnants($auto_inv).}
    complete
}

on kick {
    set chan [lindex [args] 0]
    set nick [lindex [args] 1]
    if {[showj]} {echo [t kick] channel $chan}
    after idle "refresh [list $chan];refresh [list $nick]"
    if {$nick == [my_nick]} {
	echo "[kano] Type Ctrl+I to rejoin $chan"
	set last_chans [linsert $last_chans 0 $chan]
    }
}

on timer kano_timerproc

on quit {
    set onch [list]
    foreach i [channels] {
	if {[ison [nick] $i]} {lappend onch $i}
    }
    set host {[a-z0-9.*-]+\.([a-z][a-z]|com|gov|org|mil|net|edu|arpa)}
    set ip {[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+}
    set hostexp ($ip|$host)

    if {[regexp -nocase "^$hostexp $hostexp$" \
	    [lindex [args] 0]] && [get_cookie splitwin 1]} {
	set lh [string tolower [user]@[host]]
	if {![info exists splitwin($lh)]} {
	    set splitwin($lh) [list [unixtime] [lindex [lindex [args] 0] 1] \
		    [lindex [lindex [args] 0] 0] [nick]]
	}
	if {![info exists ircserver($lh)]} {
	    incr joyn(ww)
	    /raw WHOWAS [nick]
	}
	foreach i [string tolower $onch] {
	    set splitwin($lh) [list [unixtime] [lindex [lindex [args] 0] 1] \
		    [lindex [lindex [args] 0] 0] [nick]]
	    if {$last_split($i) != [lindex $splitwin($lh) 1]} {
		echo [t split] channel $i
		set last_split($i) [lindex $splitwin($lh) 1]
	    }
	    lappend splitchan($i) [list [nick] $lh [lindex $splitwin($lh) 2]]
	}
    } else {
	set onch ""
	foreach i [channels] {
	    if {[ison [nick] $i]} {
		if {[showj] && ![get_cookie jpqstatus 0]} {echo [rep [t \
			quit] %chan $i] channel $i}
		lappend onch $i
	    }
	}
	if {[get_cookie jpqstatus 0]} {echo [rep [t quit] %chan [lindex $onch \
		0]] status}
    }

    foreach i $onch {after idle "refresh [list $i]"}
    after idle "refresh [list [nick]]"
}

foreach i {200 201 202 203 204 205 206 207 208 261 262} {
    on $i {
	nerre [t trace] 
	complete
    }
}

set scount 211
while {$scount < 219} {
    on $scount {
	nerre [t line]
	complete
    }
    incr scount
}

on 219 complete

#stats replies
foreach i {211 212 213 214 215 216 217 218 219 241 242 243 244 247 248 275 } {
    on $i {
	echo [t stats] [get_cookie status]
	complete
    }
}

on 275 {
    nerre [t dline]
    complete
}

on 221 {
    nerre [t umode]
    set um $umode
    set umode [string trimleft [lindex [args] 1] +]

    if {![info exists didmode] || !$didmode} {
	set didmode 1

	set modes [get_cookie umode]
	set positive [list]
	set negative [list]

	set dir positive
	foreach char [split $modes {}] {
	    switch -- $char {
		+ {set dir positive}
		- {set dir negative}
		default {lappend $dir $char}
	    }
	}

	set req {}
	set neg {}
	foreach i $positive {
	    if {[string first $i $umode] == -1} {
		append req $i
	    }
	}

	foreach i $negative {
	    if {[string first $i $umode] != -1} {
		append neg $i
	    }
	}
	/mode [my_nick] +$req-$neg
	if {$req != "" || $neg != ""} {
	    echo "[kano] Modifying user modes: [expr {$req == "" ? "" \
		    : "+$req"}][expr {$neg == "" ? "" : "-$neg"}]" status
	}
    }
    after idle refresh status
    complete
}

on 001 {complete}
foreach i {002 003 004 005 006 007 101 102 103 104 105 106 107} {
    on $i {if !$motdq complete}
}

foreach i {250 251 252 253 254 255 265 266} {
    on $i {
	if $motdq {nerre [t lusercrap]}
	complete
    }
}

foreach i {256 257 258 259} {
    on $i {
	if $motdq {nerre [t admincrap]}
	complete
    }
}

on 263 {
    nerre [t overloaded] 
    complete
}

on 271 {
    nerre [t silenced] 
    complete
}

on 272 complete

on 280 complete

on 281 complete

on 305 {
    nerre [t notaway]
    complete
}

on 306 {
    nerre [t away]
    complete
}

on 304 {
    complete
}

on 310 {
    nerre [t helpful] 
    complete
}

on 301 {
    if {$joyn(whois) == 0} {
	if {![info exists awm([lindex [args] 1])]} {
	    set awm([lindex [args] 1]) ""
	}
	if {$doingwhois} {
	    echo [t wiaway]
	} elseif {[lindex [args] 2] != $awm([lindex [args] 1])} {
	    echo [t isaway]
	}
	set awm([lindex [args] 1]) [lindex [args] 2]
	refresh [lindex [args] 1]
    }
    complete
}


#whois stuff

on 314 {
    if {$joyn(ww) == 0} {
	echo [t wwnick]
	echo [t wwaddr]
	echo [t wwname]
    }
    complete
}

on 311 {
    if {$joyn(whois) == 0} {
	echo [t witop]
	echo [t winick]
	echo [t wiaddr]
	echo [t winame]
	set doingwhois 1
    } else {set doingwhois 0}
    complete
}

on 319 {
    if {$joyn(whois) == 0} {echo [t wichan]}
    foreach i [split [lindex [args] 2]] {
	lappend chanelz([string trimleft [string trimleft $i @] +]) [lindex \
		[args] 1]
    }
    complete
}

on 312 {
    if {($doingwhois && $joyn(whois) == 0) || (!$doingwhois && \
	    $joyn(ww) == 0)} {
	echo [t wiserv]
    }
    set ircserver([string tolower [uhost [lindex [args] 1]]]) [string \
	    tolower [lindex [args] 2]]
}

on 317 {
    if {$joyn(whois) == 0} {
	catch {echo [t wiidle]}
	if {[llength [args]] > 4} {
	    echo [t wisign]
	}
    }
    set jw $joyn(whois)
    if {$jw > 1} {incr joyn(whois) -1}
}

on 313 {
    if {$joyn(whois) == 0} {
	echo [t wioper]
    }
    complete
}


on 315 {
    if {![info exists who_time]} {set t ??} else {
	set t [expr {([clock clicks] - $who_time) / 1000.}]
    }
    if {$do_who != "0"} {
	incr do_who -1
	set ch [string tolower [lindex [args] 1]]
	echo [t hashed] channel $ch
	if {[info exists ircopschan($ch)] && [llength $ircopschan($ch)]} {
	    echo "[kano] there [s [llength $ircopschan($ch)] are is]\
		    [llength $ircopschan($ch)] oper[s [llength \
		    $ircopschan($ch)]] on $ch: [listify \
		    $ircopschan($ch)]" channel $ch
	}
	set who_time [clock clicks]
    } else {
	echo [t whoend]
    }
    set donewho([lindex [args] 1]) [clock seconds]
    complete
}

on 318 {
    set doingwhois 0
    echo [t wiend]
    if {$joyn(whois)} {incr joyn(whois) -1}
    complete
}

on 369 {
    if {$joyn(ww) == 0} {
	echo [t wwend] [get_cookie status]
    } else {
	incr joyn(ww) -1
    }
    complete
}

on 324 {
    if {[lindex [args] 2] != "" && [channels [lindex [args] 1]] != ""} \
	    {echo [t ismode] channel [lindex [args] 1]} else {echo [t ismode]}
    set chan [lindex [args] 1]
    set moz [lindex [args] 2]
    if {[lindex [args] 3] != ""} {append moz " [lindex [args] 3]"}
    if {[lindex [args] 4] != ""} {append moz " [lindex [args] 4]"}
    set chanmode([string tolower $chan]) [string trim $moz +]
    if {[string match *k* [lindex [split $moz] 0]]} {
	if {[lindex [args] 4] != ""} {
	    set_cookie chankey([lindex [args] 1]) [lindex [args] 4]
	} elseif {[lindex [args] 3] != ""} {
	    set_cookie chankey([lindex [args] 1]) [lindex [args] 3]
	}
	if {[get_cookie chankey([lindex [args] 1])] != ""} {
	    echo "[kano] Remembering key: [get_cookie chankey([string \
		    tolower [lindex [args] 1]])]" channel [lindex [args] 1]
	}
    } elseif {[get_cookie chankey([lindex [args] 1])] != ""} {
	set_cookie chankey([lindex [args] 1]) ""
	echo "[kano] Forgetting key for [lindex [args] 1]" channel [lindex \
		[args] 1]
    }
    after idle {refresh [list $chan]}
    complete
}


on 328 {
    nerr [lrange [args] 1 end]
    complete
}

on 329 {
    if {[ison [my_nick] [lindex [args] 1]]} {
	echo [t created] channel [lindex [args] 1]
    } else {
	echo [t created]
    }
    complete
}

on 331 {
    echo [t notopic] [get_cookie status]
    set chantopic([string tolower [lindex [args] 1]]) ""
    if {[get_cookie lock([lindex [args] 1])] != "" && [isop [my_nick] \
	    [lindex [args] 1]]} {
	/topic [set c [lindex [args] 1]] [get_cookie lock($c)] KV
    }
    refresh [lindex [args] 1]
    complete
}

on 332 {
    echo [t istopic] channel [lindex [args] 1]
    set chan [lindex [args] 1]
    set chantopic([string tolower $chan]) [lindex [args] 2]
    if {![info exists lastopic($chan)]} {set lastopic($chan) ""}
    catch {refresh [list $chan]}
}


on 333 {
    echo [t settopic] channel [lindex [args] 1]
    complete
}

on 341 {
    echo [t inviting]
    complete
}

on 342 {
    echo [t summoning]
    complete
}

on 351 {nerre [t atversion]}

proc checksplit {cmd index op} {
    upvar 1 $cmd set
    set lc [string tolower $index]
    global splitchan
    if {[info exists splitchan($lc)]} {
	set l [list]
	foreach i $splitchan($lc) {
	    if {[lindex $i 2] != $set($index)} {
		lappend l $i
	    }
	}
	set splitchan($lc) $l
    }
}

trace variable last_join w checksplit

on 352 {
    set arg [lrange [args] 1 end]
    set who([string tolower [lindex [args] 5]]) [lindex [args] 2]@[lindex \
	    [args] 3]
    #for my own reference: 
    #Nick #Xircon keith gee.cs.oswego.edu irc.voicenet.com kano G {4 blah}
    set j 0
    if {[info exists a]} {unset a}
    set a(0) 9
    set a(1) 7
    set a(2) 20
    set a(3) 20
    set a(4) 8
    set a(5) 2
    set a(6) 102
    foreach i $arg {
	if {[catch {set $j [string range $i 0 $a($j)]}]} {break}
	incr j
    }
    set az ""
    set 7 [join [lrange [split $6] 1 end]]
    lappend az [lindex [split $6] 0]
    set 6 [lindex $az 0]

    set nick [string tolower [lindex [args] 5]]
    if {[info exists yesdoinv($nick)]} {
	set uhost [join [lrange [args] 2 3] @]
	if {[string match [string tolower $yesdoinv($nick)] [string tolower \
		$nick!$uhost]]} {
	    lappend definv $nick
	} else {echo "[kano] $nick's uhost ($uhost) does not match\
		$yesdoinv($nick); not asking for invite"}
	unset yesdoinv($nick)
    }
    set swl [string tolower [lindex [args] 2]@[lindex [args] 3]]
    if {[info exists splitwin($swl)] && [info exists ircserver($swl)] && \
	    [info exists joinecho($swl)]} {
	if {$ircserver($swl) == [lindex [args] 4] && [lindex $splitwin($swl) \
		1] != $last_join([string tolower [lindex $joinecho($swl) \
		1]])} {
	    set lc [string tolower [lindex $joinecho($swl) 1]]
	    echo [t rejoin] channel [lindex $joinecho($swl) 1]
	    set last_split($lc) [list]
	    set last_join($lc) [lindex $splitwin($swl) 1]
	    unset splitwin($swl)
	} elseif {$ircserver($swl) != [lindex [args] 4]} {
	    echo [lindex $joinecho($swl) 0] channel [lindex $joinecho($swl) 1]
	    unset joinecho($swl)
	}
    } else {
	if {[info exists joinecho($swl)]} {
	    echo [lindex $joinecho($swl) 0] channel [lindex $joinecho($swl) 1]
	    unset joinecho($swl)
	} elseif {!$do_who} {echo [t who]}
    }
    set ircserver($swl) [lindex [args] 4]
    set n [string tolower [lindex [args] 5]]
    set irchops($n) [lindex [split [lindex [args] 7]] 0]
    set ch [string tolower [lindex [args] 1]]
    if {![info exists ircopschan($ch)]} {set ircopschan($ch) [list]}
    if {[string first * [lindex [args] 6]] != -1 \
	    && [lsearch -exact $ircopschan($ch) $n] == -1} {
	lappend ircopschan($ch) $n
    }
    catch {unset a}
}


on 353 {
    if {$joyn(users) == "0" || [get_cookie shownames 0]} {
	nerre [t users]
    }
    complete
}

on 362 {
    nerr [arg]
    complete
}

on 363 {
    nerr [arg]
    complete
}

on 364 {add_link [lindex [args] 1] [lindex [args] 2] [lindex [split [lindex \
	[args] 3]] 0]}
on 365 {catch_links;show_links}

on 366 {
    if {$joyn(users) != 0} {
	incr joyn(users) -1
	if {[get_cookie shownames 0] && $joyn(users) == 0} {echo [t nameend]}
    }
    if {[llength [chanlist [lindex [args] 1]]] == 1 && [get_cookie newmode \
	    stn] != "" && [isop [my_nick] [lindex [args] 1]]} {
	/mode [lindex [args] 1] [get_cookie newmode stn]
    } elseif {[get_cookie opsonjoin]} {
	if {[lsearch -exact [string tolower $doaskops] \
		[string tolower [lindex [args] 1]]] != -1} {
	    /ops [lindex [args] 1]
	}
    }
    complete
}

on 368 {
    if {$joyn(ban) != "0"} {
	incr joyn(ban) -1
    } else {
	set find_ban 0
	echo [t banend]
    }
    complete
}

on 371 {
    nerr [lindex [args] 1]
    complete
}

on 375 {
    if {$motdq} {nerr [lindex [args] 1]}
    complete
}

on 372 {
    if {$motdq} {nerr [string trimleft [lindex [args] 1] -]}
    complete
}

on 376 {
    if {$motdq} {nerr [lindex [args] 1]} else {set motdq 1;echo \
	    "[kano] Skipped MOTD" status}
    complete
}

on 265 {
    if {$motdq} {nerr [lindex [args] 1]}
    complete
}

on 266 {
    if {$motdq} {nerr [lindex [args] 1]}
    complete
}
    

on 377 {
    nerr [arg]
    complete
}

on 381 {
    echo [t nowoper]
    complete
}

on 382 {
    echo "[kano] Rehashing [lindex [args] 1]..."
    complete
}

on 391 {
    nerre [t time]
    complete
}

on 392 {
    nerre [t user]
    complete
}

on 393 {
    nerre [t user]
    complete
}

on 394 complete

on 395 {
    nerre [t user]
    complete
}

on 401 {
    set n [string tolower [lindex [args] 1]]
    set isnotify 0
    for {set x 0} {$x < [user_count]} {incr x} {
	if {[string first $n [string tolower [get_user $x mask]]] == 0} \
		{set isnotify 1;break}
    }
    if {!$isnotify || [queries [lindex [args] 1]] != "" && ![string match \
	.*. [lindex [args] 1]]} {
	set t [t nosuchnick]
	if {[window name] != [lindex [args] 1]} {echo $t}
	echo $t query [lindex [args] 1]
    }
    catch {unset who([string tolower [lindex [args] 1]])}
    catch {refresh [list [lindex [args] 1]]}
}


on 402 {
    echo [t nosuchserver]
    complete
}

on 403 {
    echo [t invalidchan]
    complete
}

on 404 {
    if {[ison [my_nick] [lindex [args] 1]]} {
	echo [t cannotsendmoderated] channel [lindex [args] 1]
    } else {
	echo [t cannotsendmoderated]
    }
    complete
}

on 405 {
    echo [t toomanychans]
    complete
}

on 406 {
    if {$joyn(ww) == 0} {
	echo [t wasnonick]
    }
    complete
}

on 407 {
    erre [t occurs]
}


on 409 {
    erre [t pingorigin]
    complete
}

on 411 {
    nerr [lindex [args] 1]
    complete
}

on 412 {
    echo [t notext]
    complete
}

on 413 {
    erre [t notoplvl]
    complete
}

on 414 {
    erre [t topwild]
    complete
}

on 421 {
    if {[string tolower [lindex [args] 1]] != "kanoremote"} {
	echo [t cmdunknown]
    }
    complete
}

on 422 {
    if {[get_cookie motd] && $motdq} {set motdq 0}
    complete
}

on 423 {
    erre [t noadmin]
    complete
}

on 424 {
    err [lindex [args] 1]
    complete
}

on 431 {
    echo [t nonickgiven]
    complete
}

on 432 {
    echo [t badnick]
    complete
}

on 433 {
    if {[string tolower [lindex [args] 1]] != [string tolower $getnick]} {
	echo [t nickinuse]
	if {![connected] && [get_cookie autogetnick 1]} {/getnick [lindex \
		[args] 1]}
    }
    complete
}

on 436 {
    erre [t nickcollision]
    complete
}

on 437 {
    err [lrange [args] 1 end]
    complete
}

on 441 {
    if {$onchanQ <= 0} {echo [t notonchan] channel [lindex [args] 2];set \
	    onchanQ 0} else {incr onchanQ -1}
    complete
}

on 442 {
    echo [t notinchan]
    complete
}

on 443 {
    echo [t alreadyonchan]
    complete
}

on 444 {
    erre [t notlogged]
    complete
}

on 445 {
    erre [t summondisabled]
    complete
}

on 446 {
    erre [t usersdisabled]
    complete
}

on 451 {
    erre [t notregistered]
    complete
}

on 461 {
    erre [t toofewparms]
    complete
}

on 462 {
    erre [t alreadyregistered]
    complete
}

on 463 {
    erre [t notprivileged]
    complete
}

on 464 {
    erre [t badpass]
    complete
}

on 465 {
    erre [t bannedfromserver]
    complete
}

on 467 {
    echo [t keyset]
    complete
}

on 471 {
    echo [t chanfull]
}


on 472 {
    if {[lindex [args] 1] != "e"} {
	echo [t unknownmode]
    }
    complete
}

on 473 {
    echo [t chaninvite]
}

on 474 {
    echo [t chanbanned]
}

on 475 {
    echo [t chanbadkey]
}

on 478 {
    echo [t banlistfull] channel [lindex [args] 1]
    complete
}

on 481 {
    echo [t nopermission]
    complete
}

on 482 {
    echo [t notop] channel [lindex [args] 1]
    complete
}

on 483 {
    erre [t serverkill]
    complete
}

on 491 {
    erre [t notoper]
    complete
}

on 501 {
    echo [t unknownmode]
    complete
}

on 502 {
    echo [t notyourmode]
    complete
}

on 511 {
    erre [t silencefull]
    complete
}

on 512 {
    erre [t nogline]
    complete
}

on 513 {
    err [lindex [args] 1]
    complete
}

on ctcp {
    set arg [args]
    set cmd [string tolower [lindex $arg 1]]
    set blah [split [lindex [args] 2]]
    if {$cmd != "action" && $cmd != "dcc" && $cmd != "sound"} {
	if {[get_cookie cloak] || ($isaway && [get_cookie autocloak])} {
	    if {[lindex [args] 0] != [my_nick]} {
		set mza " to [lindex [args] 0]"
	    } else {
		set mza ""
	    }
	    nerre [rep [t ctcpcloaked] %to $mza]
	    complete
	    return
	}
	set x [lindex [args] 0]
	if {$x == [my_nick]} {set x you}
	if {[get_cookie msglog] \
		&& $isaway \
		&& [lindex [args] 1] != "ACTION" \
		&& [lindex [args] 1] != "SOUND" \
		&& [string tolower [lindex [args] 1]] != "page"} {
	    addmlog "CTCP [join [lrange [args] 1 end]] from\
		    [nick]([user]@[host]) to $x"
	}
	if {$x == "you" && [lindex [args] 1] == "ACTION"} {
	    addmlog "* [nick] [lindex [args] 2]"
	}
	set arg [args]
	if {$cmd == "version"} {
	    /quote NOTICE [nick] :VERSION [fullvers]
	} elseif {$cmd == "sound" && [string index $arg 0] == "#"} {
	    if {[get_cookie sound(on)] && [lindex $arg 0] == [channel] && \
		    [lsearch [get_cookie nosounds] [string tolower [lindex \
		    [args] 0]]] == -1 && [lsearch [get_user [find_user \
		    [nick]![user]@[host]] notes] nosounds] == -1} {
		mmplay [file join [get_cookie sound(dir)] [lindex [args] 1]]
	    }
	    echo [t sound] channel [lindex $arg 0]
	    complete
	    return
	} elseif {$cmd == "kano"} {
	    /quote NOTICE [nick] :KANO [lindex [kanovers] 1]
	} elseif {$cmd == "page"} {
	    mmstop all
	    mmplay [get_cookie page(sound)]
	    echo [t page]
	    addmlog "[nick]([user]@[host]) paged you"
	    complete
	    return
	} elseif {$cmd == "ping"} {
	    set reply [string tolower [lindex [args] 1]]
	    if {[info exists ctcprep($reply)]} {catch $ctcprep($reply) ctcperr}
	    catch {/quote NOTICE [nick] :[lindex [args] 1] $ctcperr} \
		    ctcperr2
	}
	if {[lindex [args] 2] != "" && $cmd != "ping"} {set e1 " [lindex \
		[args] 2]"} else {set e1 ""}
	if {[lindex [args] 0] != [my_nick]} {
	    set mza " to [k chan][lindex [args] 0]"
	} else {
	    set mza ""
	}
	echo [rep [rep [t ctcp] %to $mza] %cmd [lindex [args] 1]$e1]
	complete
    }
}


on nick {
    if {[nick] == [my_nick]} {after idle "refresh status"} else {after idle \
	    [list refresh [lindex [args] 0]]}
    catch {
	set said([string tolower [lindex [args] 0]]) $said([string tolower \
		[nick]])
	unset said([string tolower [nick]])
    }
    set lfrom [string tolower [nick]]
    set lto [string tolower [lindex [args] 0]]
    if {$lto != $lfrom && [info exists who($lfrom)]} {
	set who($lto) $who($lfrom)
	catch {unset who($lfrom)}
    }
    set onch ""
    if {[nick] == [my_nick]} {
	foreach i [channels] {
	    echo [t mynick] channel $i
	}
	echo [t mynick] status
    } else {
	foreach i [channels] {
	    if {[ison [nick] $i]} {
		echo [t nick] channel $i
		lappend onch $i
	    }
	}
    }
}

on join {
    set echo [t join]
    set n [string tolower [nick]]
    set chan [string tolower [lindex [args] 0]]
    set chanjoin($chan) [nick]
    set h [string tolower [host]]
    set c [lindex [args] 0]
    set lc $chan
    set ln $n
    set lh [string tolower [user]@[host]]
    set who($ln) $lh
    if {[nick] == [my_nick]} {
	set locknick($lc) ""
	set chantopic($lc) ""
	set chanbans($lc) ""
	set chanex($lc) ""
	incr joyn(ban)
	incr joyn(users)
	incr joyn(mode)
	incr joyn(ex)
	/raw mode $lc e
	set last_join($lc) asdfadfsf
	set last_split($lc) asdfafsdf
    }
    if {[nick] == [my_nick]} {
	foreach nick [queries] {after idle [list refresh $nick]}
    }
    if {![info exists gotkicked($lc)]} {set gotkicked($lc) 0}
    set chan [string tolower [lindex [args] 0]]
    set lh [string tolower [user]@[host]]
    after idle "refresh [list [lindex [args] 0]];refresh [list [nick]]"
}


on part {
    if {[my_nick] == [nick]} {
	catch {echo "[kano] left [lindex [args] 0] in [mssec [expr \
		{[clock clicks] - $parted([string tolower [lindex [args] \
		0]])}]]"}
	foreach nick [queries] {after idle [list refresh $nick]}
    } else {
	if {[get_cookie jpqstatus 0]} {echo [t part] status} \
		elseif {[showj]} {echo [t part] channel [lindex [args] 0]}
    }
    after idle "refresh [list [lindex [args] 0]];refresh [list [nick]]"
}

on mode+k {
    set_cookie chankey([string tolower [lindex [args] 0]]) [lindex [args] 1]
}

on mode-e {
    set x $chanex([string tolower [lindex [args] 0]])
    set ban [lindex [args] 1]
    catch {set chanex([string tolower [lindex [args] 0]]) [lreplace $x \
	    [set s [lsearch -exact $x $ban]] $s]}
}

on mode {
    if {[nick] == ""} {set nq [host]} else {set nq [nick]}

    set lchan [string tolower [lindex [args] 0]]

    if {![info exists lastmode($lchan)]} {set \
	    lastmode($lchan) ""}
    if {[raw_args] != $lastmode($lchan) || \
	    ![get_cookie suppress_mode 1]} {
	set m " [join [lrange [args] 2 end]]"
	if {$m == " "} {set m ""}
	echo [rep [rep [t modechange] %nick $nq] %mode $m] channel [lindex \
		[args] 0]
    }
    set lastmode($lchan) [raw_args]
    lappend refresh_chan [lindex [args] 0]
    complete
}

on lookup {
    set request [lindex [args] 0]
    set result  [lindex [args] 1]
    if {$result == ""} {
	set 0 [string tolower $request]
	set 1 $0
    } else {
	set 0 [string tolower $result]
	set 1 [string tolower $request]
    }
    set dnsinverse($1) $0
    if {[info exists dnswaiting($1)]} {complete;return}
    set m0 ""
    set m1 ""
    set m2 ""
    foreach i [array names who] {
	set t [string tolower [lindex [split $who($i) @] 1]]
	if {$t == $request} {lappend m1 $i}
	if {$t == $result} {lappend m2 $i}
	if {[info exists dnsreq($result)]} {
	    if {$t == $dnsreq($result)} {lappend m0 $i}
	}
    }
    if {$result != ""} {
	if {![info exists dnsreq($request)]} {
	    set dnsreq($result) $request
	    lookup $result
	} else {
	    if {$dnsreq($request) == $result} {
		echo [t dnsfound]
	    } else {
		echo [t dnsrelay]
	    }
	    unset dnsreq($request)
	}
    } else {
	if {[info exists dnsreq($request)]} {
	    echo [t dnsrevfail]
	    unset dnsreq($request)
	} else {
	    echo [t dnsfail]
	}
    }
}


on wallops {
    echo "[kano] wallops([nick]): [join [args]]"
    complete
}

on 367 {
    lappend chanbans([string tolower [lindex [args] 1]]) [lindex [args] 2]
    if {$joyn(ban) == 0} {
	if {[string match [strep [string tolower [lindex [args] 2]]] [string \
		tolower [my_nick]![my_user]@[my_host]]]} {
	    echo [t bans]
	} else {
	    nerre [t bans]
	}
    }
    complete
}


on 348 {
    lappend chanex([string tolower [lindex [args] 1]]) [lindex [args] 2]
    if {$joyn(ex) == 0} {echo [t except]}
    complete
}

on 349 {
    if {$joyn(ex)} {incr joyn(ex) -1} else {echo [t exceptend]}
    complete
}

on ctcp {
    if {[string tolower [lindex [args] 1]] == "action"} {
	if {[lindex [args] 0] != [my_nick]} {
	    echo [rep [t action] %text [lindex [args] 2]] channel [lindex \
		    [args] 0]
	    lappend said([string tolower [nick]]) [list [lindex [args] 0] \
		    [lindex [args] 2] [nick] action]

	} else {
	    if {[FALC_config preferences\\general createqueries 1] && \
		    [queries \
		    [nick]] == ""} {
		if {![window exists query .Messages.]} {
		    set name [nick]
		    set state [window state [window type] [window name]]
		    set win [list [window type] [window name]]
		    /query [nick]
		    if {[FALC_config preferences\\general minimizequeries 0]} {
			window minimize query [nick]
			window focus [lindex $win 0] [lindex $win 1]
			window $state [window type] [window name]
		    }
		} else {
		    set name .Messages.
		}
		echo [rep [t action] %text [lindex [args] 2]] query $name
	    } elseif {[queries [nick]] != ""} {
		echo [rep [t action] %text [lindex [args] 2]] query [nick]
	    } elseif {![FALC_config preferences\\general createqueries 1] && \
		    [queries [nick]] == ""} {
		if {![window exists query .Messages.]} {
		    set chan ""
		    set name ""
		} else {
		    set chan query
		    set name .Messages.
		}
		echo [rep [t actionnoquery] %text [lindex [args] 2]] $chan \
			$name
	    }
	}
	complete
    }
}

on disconnect {
    set lastdis [unixtime]
    foreach i [channels] {refresh $i}
    complete
}

on unload {
    foreach i [array names dlfile] {catch {close $i}}
    catch {close $apc}
    foreach i [array names sock] {catch {close $sock($i)}}
    catch {close $mailv}
    catch {close $identerr}
    catch {close $nukeerr}
    catch {close $porterr}
}


on notice {
    if {!$ischan} {add_tabnick [nick]}
    if {[get_cookie msglog] && $isaway && !$ischan && [host] != "" && \
	    [nick] != ""} {addmlog "0-[nick]![user]@[host]0- [lindex \
	    [args] 1]"}
    if {[string match "*Received*KILL*message*for*" [lindex [args] 1]] && \
	    [nick] == ""} {
	nerre [t killed]
	complete
	return
    }
    set str [lindex [args] 1]
    if {[nick] == ""} {
	echo [rep [t servnotice] %text [string trimleft $str "* "]] status
    } else {
	set stz [string tolower [strip [lindex [split $str] 0]]]
	set GO 1
	if {$ischan} {
	    if {$char == ""} {
		set t [t pubnotice]
	    } else {
		set t [t wallnotice]
	    }
	    echo [rep $t %text $str] channel $d
	    complete
	    set GO 0
	} elseif {[regexp -nocase {(op|wall|@).*#} $stz]} {
	    set iswall 0
	    foreach i [string tolower [channels]] {
		if {[string first $i $stz] != -1} {
		    echo [rep [t notice] %text $str] channel $i
		    set iswall 1
		}
	    }
	    if {$iswall} {
		set GO 0
		add_tabnick /wall
		complete
	    }
	}
	if {$GO} {
	    set m 0
	    foreach i [channels] {
		if {[ison [nick] $i]} {
		    echo [rep [t notice] %text $str] channel $i
		    set m 1
		}
	    }
	    if {!$m} {echo [rep [t notice] %text $str]}
	}
    }
    complete
}

on privmsg {
    set clix2 [clock clicks]
    set lastprivmsg($n) [lindex [args] 1]
    set m [list [unixtime] $d [lindex [args] 1] [nick]]
    set msg [lindex [args] 1]
    set targ $d
    set string $msg
    if {$ischan} {
	set arg [args]
	set say $string
	set yes 0
	set no 0
	set s2 [string tolower $say]
	foreach i [get_cookie hilite(not)] {
	    if {[string first $i $s2] != -1} {set no 1;break}
	}
	if {!$no} {
	    foreach i [string tolower [get_cookie hilite(words)]] {
		if {[string first $i $s2] != -1} {
		    incr yes
		    break
		}
	    }
	    set lm [string tolower [my_nick]]
	    if {[get_cookie hilite(nick) 1]} {
		if {[string first $lm $s2] != -1} {incr yes} 
	    }
	}
	set yes2 $yes
	if {$yes} {
	    echo [rep [t highlight] %text $say] channel $d
	    mmplay [get_cookie sound(nick)]
	    addmlog "0<[nick]![user]@[host]:0[lindex $arg 0]> $say"

	    set switch [get_cookie hilite(switch)]
	    if {$switch == 1 && "channel $l" != [string \
		    tolower "[window type] [window name]"]} {
		set chan $d
		if {[window state channel $chan] == "minimize"} {
		    window restore channel $d
		}
		if {[active]} {window focus channel $d}
	    } elseif {$switch == 2 && "channel [string \
		    tolower $d]" != "[window type] [string tolower [window \
		    name]]"} {
		echo "\[$char$d\] [rep [t highlight] %text $say]"
	    }

	    set f [lsearch -exact $lastniq [nick]]
	    if {$f != "-1"} {
		set lastniq [lreplace $lastniq $f $f]
	    }
	    lappend lastniq [nick]
	    return
	}
	if {![info exists last_said([nick])]} {set last_said([nick]) [list 0 \
		[list]]}
	if {![get_cookie suppress_text 0] || ($string != [lindex \
		$last_said([nick]) 1] || [clock seconds] - [lindex \
		$last_said([nick]) 0] > 10)} {
	    if {$char == ""} {
		set t [t pubmsg]
	    } else {
		set t [t wallmsg]
	    }
	    echo [rep $t %text $string] channel $targ
	    set last_said([nick]) [list [clock seconds] $string]
	    echo "([cform "%I:%M:%S"][string tolower [cform %p]],[lindex \
		    [args] 0]) <[nick]> [lindex [args] 1]" query \
		    .said([string tolower [nick]]).
	}
    } else {
	set ex [window exists query [nick]]
	set q [FALC_config preferences\\general createqueries 1]
	set msg [window exists query .Messages.]
	if {!$ex && $q} {
	    if {!$msg} {
		set name [nick]
		set state [window state [window type] [window name]]
		set win [list [window type] [window name]]
		/query [nick]
		if {[FALC_config preferences\\general minimizequeries 0]} {
		    window minimize query [nick]
		    window focus [lindex $win 0] [lindex $win 1]
		    window $state [window type] [window name]
		}
	    } else {
		set name .Messages.
	    }
	    set last_privmsg [rep [t msg] %text $string]
	    echo $last_privmsg query $name
	} elseif {!$q && !$ex} {
	    if {!$msg} {
		set chan ""
		set name ""
	    } else {
		set chan query
		set name .Messages.
	    }
	    set last_privmsg [rep [t msgnoquery] %text $string]
	    echo $last_privmsg $chan $name
	    add_tabnick [nick]
	    if {!$ischan && [queries [nick]] == ""} {
		lappend relaytext([string tolower [nick]]) $last_privmsg
	    }
	} else {
	    set last_privmsg [rep [t msg] %text $string]
	    echo $last_privmsg query [nick]
	}
    }
}


on chat_send {
    if {[string match ACTION* [lindex [args] 0]]} {
	echo [rep [t myaction] %text [string range [set ra [string range \
		[raw_args] 1 end]] [expr {[string first " " $ra] +1}] end]] \
		chat [nick]
    } else {
	echo [rep [t mydccmsg] %text [string range [raw_args] 1 end]] chat [nick]
    }
    complete
}

on chat_text {
    if {[string match ACTION* [lindex [args] 0]]} {
	echo [string trimright [rep [t dccact] %text [string range [set ra \
		[string range [raw_args] 1 end]] [expr {[string first " " \
		$ra] +1}] end]] ] chat [nick]
    } else {
	echo [rep [t dcctext] %text [string range [raw_args] 1 end]] chat \
		[nick]
    }
    complete
}

on connect {
    set umode ""
    mmstop all
    mmplay [get_cookie sound(connect)]
    set serverlag ??
    catch {unset who}
    foreach element [array names splitwin] {
	unset splitwin($element) ;#to register the variable trace
    }
    set do_who 0
    set joyn(ban)   0
    set joyn(users) 0
    set joyn(mode)  0
    set joyn(whois) 0
    set joyn(ex)    0
    set current_net [getnet [lindex [args] 0]]
    set didmode 0
    if {![get_cookie fixednetajoin 0]} {
	set_cookie fixednetajoin 1
	set_cookie ajoin([getnet]) [get_cookie ajoin]
    }
    if {[get_cookie ajoin()] != ""} {
	if {[get_cookie ajoin([getnet])] == ""} {set_cookie ajoin([getnet]) \
		[get_cookie ajoin()];echo "[kano] Converted autojoin for net\
		[getnet]"}
	set_cookie ajoin() ""
    }
    if {[lsearch [set serverz [get_cookie servers]] [raw_args]] == "-1"} {
	lappend serverz [server]
	set_cookie servers $serverz
    }
    if {$isaway} {/quote AWAY :$awayr}
    if {[get_cookie msglog] && $isaway} {addmlog \
	    "Connected to [join [args] :]"}
    if {[string trim [get_cookie ajoin([curnet])] ,] != "" && ($whoami == \
	    1 || [get_cookie ajoin2 0]) && [get_cookie ajoin_ok 1]} {
	set x [split [get_cookie ajoin([curnet])] ,]
	set m ""
	foreach i $x {
	    if {[channels $i] == ""} {lappend m $i}
	}
	if {$m != ""} {
	    /join [join $m ,]
	}
    }
    set_cookie avgping ""
    if {![get_cookie motd]} {set motdq 0}
    set lastserver [args]
    set lastident 0
    set nonRFC 0
    set_cookie onlinet [clock seconds]
    echo "[kano] Connected to [lindex [args] 0] ([getnet \
	    [lindex [args] 0]])" status
    set lserver [args]
    mmplay [get_cookie sound(connect)]
    set lastping [expr {[unixtime]-61}]
    set interver 1
    if {[catch [get_cookie perform] x]} {echo \
	    "[kano] Error in perform script: $x\n$errorInfo"}
}

on pong {
    if {[string range [lindex [args] 1] 0 7] == "LAGMETER"} {
	set serverlag [string range [lindex [args] 1] 8 end]
	set serverlag [expr {([clock clicks] - $serverlag)/1000.}]
	refresh status
	if {$show_ping} {
	    echo "[kano] Lag: ${serverlag}secs"
	    set show_ping 0
	}
    }
}


on error {
    echo "[kano] [string range [raw_args] 1 end]" status
    complete
}

on load complete

on timer {
    set fix [list]
    foreach name [array names relaytext] {
	lappend fix [list fix_rel_len relaytext $name]
    }
    thread_every fix_relay 60 $fix

    set fix [list]
    foreach name [array names said] {
	lappend fix [list fix_rel_len said $name]
    }
    thread_every fix_said 60 $fix
}

proc killsplit {array i op} {
    global splitchan
    foreach j [array names splitchan] {
	set e [list]
	foreach k $splitchan($j) {if {[lindex $k 1] != $i} {lappend e $k}}
	if {$e != $splitchan($j)} {
	    set splitchan($j) $e
	    refresh $j
	}
    }
}

trace variable splitwin u killsplit

on_minute {
    set echolines [get_cookie echolines 100]
} {
    foreach i [array names splitwin] {
	if {[unixtime] - [lindex $splitwin($i) 0] >= 600} {
	    unset splitwin($i)
	}
    }
} {
    if {$env(kanoidle) > [expr {[get_cookie autoaway 0]*60}] && !$isaway \
	    && [connected] && [get_cookie autoaway 0] != 0} {
	set temp $env(kanoidle)
	/away autoaway/[since [expr {[get_cookie autoaway]*60}]]
	set isaway 1
	incr away -$temp
    }
    set un [unixtime]
    foreach i [array names unignore] {
	if {$unignore($i) <= $un} {
	    echo [rep [t unignore] %host $i] [get_cookie status]
	    unset unignore($i)
	    /ignr $i none
	}
    }
} {
    if {[osuptime] > [get_cookie winuptime 0]} {set_cookie winuptime \
	    [osuptime]}
}

###hotkeys

hotkey enter {
    set temp_nix 0
    if {[clock format [get_cookie lastret 0] -format "%m%d%y"] != [clock \
	    format [clock seconds] -format "%m%d%y"] && ([catch {sockon \
	    $env(KMOTD)} sockz] || $sockz == 0)} {after idle grab_motd}
    set_cookie lastret [clock seconds]
}

hotkey control+i {
    if {[llength $last_chans] == 1} {
	/join [lindex $last_chans 0]
    } {
	input set_text "/join [lindex $last_chans 0]"
	input set_sel_start [string length "/join [lindex $last_chans 0]"]
	set last_chans "[lrange $last_chans 1 end] [lindex $last_chans 0]"
    }
    complete
}
hotkey control+f {
    foreach {chan val} [array get splitchan] {
	if {![llength $val]} {continue}
	echo "[kano] ${litehelp}$chan"
	foreach nick [lsort -command [list sortindex 0] $val] {
	    echo "      $litehelp[lindex $nick 0] ${darkhelp}([lindex \
		    $nick 1]) 14\[[lindex $nick 2]\]"
	}
    }

    complete
}

hotkey tab {
    set text [input get_text]
    set doin_da_tab 0
    if {[llength [split [string trim $text]]] == 0} {
	set temp_nix 0
	set doin_da_tab 1
    } elseif {[string trim $text] == [string trim $last_tab]} {
	incr temp_nix
	if {[lindex [lrange $tab_nix $temp_nix $temp_nix] 0] == ""} {
	    set temp_nix 0
	}
	set doin_da_tab 1
    }
    if $doin_da_tab {
	input set_text "[string trim [lindex [lrange $tab_nix $temp_nix \
		$temp_nix] 0]] "
	input set_sel_start 512
	set last_tab [input get_text]
	complete;return
    }
}
hotkey tab {
    #basics:
    set txt [input get_text]
    set st [input get_sel_start]

    #what word is the cursor in?
    set start [wordstart $txt [expr {$st -1}]]

    #is it a command?
    if {$start == 0 && [string index $txt 0] == "/"} return

    #needs to be completed:
    set needs [string range $txt $start end]
    set newneeds ""
    for {set i 0} {$i < [string length $needs]} {incr i} {
	set char [string index $needs $i]
	if {$char == " "} break {append newneeds $char}
    }
    set needs $newneeds

    #are they nick-cycling?
    if {$needs == ""} {complete;return}
    if {[string tolower $needs] == [string tolower [lindex $comped 0]]} {
	if {[lindex $comped 1] == "" || [string tolower [lindex $comped \
		1]] == [string tolower $needs]} {
	    set comped [list [lindex $comped 0] $all_nix]
	}
	set completed [lindex [lindex $comped 1] 0]
	set comped [list $completed [lrange [lindex $comped 1] 1 end]]
    } {
	#set the last person you tabbed..confusing so don't bother
	if [info exists completed] {
	    if {$completed != ""} {
		add_last_speak [channel] $completed
	    }
	}

	#no? then what to complete with
	if {[window type] == "channel"} {set nix [chanlist [window name]]} {
	    set nix ""
	    foreach i [channels] {append nix "[chanlist $i] "}
	    foreach i "[queries] [chats]" {append nix "$i "}
	    lappend nix [my_nick]
	}

	#complete it!@$%$#@#
	set completed ""
	set all_nix ""
	set break 0
	foreach i [lsort $nix] {
	    if [string match [string tolower [strep $needs]]* [string \
		    tolower $i]] {
		if !$break {set completed $i} elseif {[lsearch $all_nix \
			$i] == -1} {lappend all_nix $i}
		set break 1
	    }
	}

	#try stripping the punctuation
	foreach i [lsort $nix] {
	    if [string match [string tolower [strep $needs]]* [string \
		    trimleft [string tolower $i] {`^-_[]{}\|}]] {
		if {!$break && $completed == ""} {set completed $i} \
			elseif {[lsearch $all_nix $i] == -1} {lappend \
			all_nix $i}
		set break 1
	    }
	}

	#no? then assume $needed not the beginning of the nick but in the
	#middle
	if {$completed == "" && [string length $needs] > 3} {
	    foreach i [lsort $nix] {
		if [string match *[string tolower [strep $needs]]* [string \
			tolower $i]] {
		    if {!$break && $completed == ""} {set completed \
			    $i} elseif {[lsearch $all_nix $i] == \
			    -1} {lappend all_nix $i}
		    set break 1
		}
	    }
	}
	
	#no? maybe it's a channel
	if {$completed == ""} {
	    foreach i [lsort [channels]] {
		if {[string match *[string tolower $needs]* [string \
			tolower $i]]} {
		    lappend all_nix $i
		}
	    }
	    set completed [lindex $all_nix 0]
	    set all_nix [lrange $all_nix 1 end]
	}

	#no? maybe it's a caught url
	if {$completed == ""} {
	    foreach i $urls {
		if [string match *[string tolower $needs]* [string \
			tolower $i]] {
		    lappend all_nix $i
		}
	    }
	    set completed [lindex $all_nix 0]
	    set all_nix [lrange $all_nix 1 end]
	}

	#no? maybe it's a filename
	if {$completed == ""} {
	    set f [lsort [glob -nocomplain -- [string tolower $needs]*]]
	    set e ""
	    foreach i $f {
		if {[string first " " $i] != -1} continue
		if [file isdir $i] {set i $i/}
		lappend e $i
	    }
	    set f $e
	    set all_nix [concat $all_nix [lrange $f 1 end]]
	    set completed [lindex $f 0]
	}

	#no?? oh well :|
	if {$completed == ""} {complete;return}
	
	#sort them by who the last to speak was (done in some privmsg event)
	set all_nix [concat [list $completed] $all_nix]
	set an $all_nix
	set all_nix [revorder [lsort -command [list last_speak \
		[channel]] [lsort -decreasing $all_nix]]]
	set all_nix [killdupe $all_nix]
	set completed [lindex $all_nix 0]
	set all_nix [lrange $all_nix 1 end]
	set an ""
	foreach i $all_nix {
	    if {[lsearch -exact $an $i] == -1} {lappend an $i}
	}
	set all_nix $an

	#set up nick cycling
	set comped [list $completed $all_nix]
	set all_nix "[list $completed] $all_nix"
    }

    #if caps lock is on, complete it in caps
    if [FALC_lock caps] {set completed [string toupper $completed]}

    #insert the fixed nick
    input set_text "[string range $txt 0 [expr {$start \
	    -1}]]$completed[string range $txt [expr {$start+[string \
	    length $needs]}] end]"
    input set_sel_start [expr {$start+[string length $completed]}]
    complete
}


hotkey escape {
    if [info exists "readz([window type] [window name])"] {
	/play -stop
	complete
    }
}


hotkey control+g {
    if [info exists request] {
	FALC_clip -c $request
	echo "[kano] Copied $request to clipboard."
	complete
    }
}

hotkey f7 {
    if !$marqing {
	set marqing 1
	marq [input get_text] 1
    } {
	set marqing 0
    }
}


for {set i 1} {$i < 10} {incr i} {
    hotkey alt+$i "set_win $i;complete"
}

hotkey alt+left {
    change_window -1
}

hotkey alt+right {
    change_window
}

####end of necessities

loadmsg

foreach lmodule [lsort $mods] {
    loadmsg "Loading modules"
    catch {unset modinfo}
    set startload [clock clicks]
    set this_addon [file root [file tail $lmodule]]
    set this_full_addon $lmodule
    if {[catch {source $lmodule} er] == 1} {
	addecho "[kano] Error loading [file tail $lmodule]: $er\n$errorInfo"
	lappend modErrors $lmodule
    } else {
	addecho "[kano] Loaded [expr {[info exists \
		modinfo(name)] ? $modinfo(name) : [file tail \
		$lmodule]}] ver [expr {[info exists \
		modinfo(ver)] ? $modinfo(ver) : "??"}] in [mssec [expr \
		{[clock clicks] - $startload}]][expr {[info exists \
		modinfo(desc)] ? ": $modinfo(desc)" : ""}]"
    }
    set this_addon {}
    set this_full_addon {}
}
echo $AddEcho status
set modtime [expr {[clock clicks] - $modtime}]
set LOADING 0
loadmsg

foreach i [array names help] {
    if {[string index $i 0] == "+" || [string index $help($i) \
	    0] == "!"} continue
    lappend helpme([lindex $help($i) end]) $i
}

set usedch {}

foreach i [lsort [array names helpme]] {
    set char 0
    while {[lsearch -exact $usedch [string index $i $char]] != -1} {
	incr char
	if {[string index $i $char] == ""} break
    }
    lappend usedch [string index $i $char]
    set beg [expr {$i == "" ? "Basic commands" : "addon: [string range \
	    $i 0 [expr {$char-1}]]&[string range $i $char end]"}]->
    set ok [expr {[llength $helpme($i)] > 10}]
    foreach j [lsort $helpme($i)] {
	if {$ok} {
	    set b2 &[string index $j 0]->
	} else {set b2 {}}
	lappend helpmenu [list $beg$b2$j "/help $j"]
    }
}
foreach win {status channel query chat} {
    foreach i $helpmenu {
	menu $win &Help->&Aliases->[lindex $i 0] [lindex $i 1]
    }
}

####compatibility with privmsg.ka

on join {
    add_last_speak [lindex [args] 0] [nick]
    if {[info exists splitwin($lh)]} {
	if {[lindex $splitwin($lh) 2] != [lindex $last_join($chan) 0]} {
	    set joinecho($lh) [list $echo [lindex [args] 0]]
	    /quote who [nick]
	    set last_join($chan) [lindex $splitwin($lh) 2]
	} else {unset splitwin($lh)}
    } else {
	if {[nick] != [my_nick]} {
	    if {[string trim [set hostx [lrange [split [host] .] end end]] \
		    1234567890] != "" && [string length $hostx] == 2} {
		if {[showj]} {echo [t joincountry] channel [lindex [args] 0]}
	    }
	    if {[get_cookie jpqstatus 0]} {echo $echo status} \
		    elseif {[showj]} {echo $echo channel [lindex [args] 0]}
	} else {
	    if {[window exists channel [lindex [args] 0]]} {set k \014} \
		    else {set k ""}
	    set onEcho([string tolower [lindex [args] 0]]) $k[t myjoin]
	}
    }
}

####end of damn slow processors

####more fun stuff

foreach i {status channel users chat query} {
    foreach j $addmenu {
	if {[string match [lindex $j 0] $i]} {
	    menu $i &Addons->[lindex $j 1] [lindex $j 2]
	}
    }
}

####end of even more messiness
foreach i {001 002 003 004 005 006 007 101 102 103 104 105 106 107 200 \
	201 202 203 204 205 206 207 208 211 212 213 214 215 216 217 218 \
	219 221 241 242 243 244 247 248 250 251 252 253 254 255 256 257 \
	258 259 261 262 263 265 266 271 272 275 280 281 302 304 305 306 \
	310 311 312 313 314 315 317 318 319 324 328 329 331 332 333 341 \
	342 348 349 351 352 353 362 363 364 365 366 367 368 369 371 372 \
	375 376 377 381 382 391 392 393 394 395 401 402 403 404 405 406 \
	407 409 411 412 413 414 421 422 423 424 431 432 433 436 437 441 \
	442 443 444 445 446 451 461 462 463 464 465 467 471 472 473 474 \
	475 478 481 482 483 491 501 502 511 512 513 chat_accept \
	chat_connect chat_disconnect chat_send chat_text connect \
	ctcp_reply dcc_begin dcc_complete dcc_create dcc_error denotify \
	disconnect error invite join kick load lookup mode nick notice \
	notify part pong privmsg quit topic unload wallops whois} {on $i \
	complete}
alias privmsg complete
set sc [expr {[clock clicks] - $loadtimer}]
foreach i {users channel query chat status} {
    if {![info exists menuitems($i)]} {
	foreach j [list "No addons or modules" "have created menu items" \
		"for this window type" "" "Try loading the popups" \
		"module or download it at" "" "*www.kano.net"] {
	    menu $i $j {exec start http://www.kano.net &}
	}
    }
}
unset menuitems

####damn conio.thm

rename isnum mynum
proc isnum n {expr {[mynum $n]}}

####end of gah

####init.tcl kinda :)

alias * {
    set cmd [string tolower [lindex [args] 0]]
    set addlist [list {bot ops bots botinv} bots.ka emacs emacs.ka \
	    {scramble slot ttt} games.ka \
	    {funban screw tkb wkick rkick repk capk clone dban} kick.ka \
	    {def translate dict spell} lookup.ka \
	    {check pop read sendmail} mail.ka \
	    {mkb mkick mdop mop undo mv mdv} mass.ka play paste.ka \
	    spellfix spellfix.ka]
    foreach {a b} $addlist {
	if {[lsearch -exact $a $cmd] != -1} {
	    echo "[kano] The /$cmd command is available by copying '[local \
		    [file join [pwd] kano {more addons} $b]]' into\
		    'xircon\\ addons'"
	    complete;return
	}
    }
}

####end of more-addons notice

echo "[kano] Loaded: script/[mssec \
	[expr {$sc - $loadaddons - $themeloaded - $dlltime - $modtime - \
	$firstload}]] addons/[mssec $loadaddons] firstload/[mssec \
	$firstload] mods/[mssec $modtime] dlls/[mssec \
	$dlltime] theme/[mssec $themeloaded] total/[mssec \
	$sc]\n[kano] Got a bug, question, suggestion, or comment?\
	Type '/bug <description of your problem/question>' and\
	kano will look into it as soon as possible. Probably even\
	today. Include your email address for a response via\
	email.\n[kano] Tip of the moment: [tip]" status

set showinfo 0
if {[info exists modErrors]} {
    set m [llength $modErrors]
    echo "[kano] 4There w[s $m ere "as an"] error[s $m] while loading\
	    $m module[s $m]: [listify $modErrors]" status
    set showinfo 1
}
if {[info exists addErrors]} {
    set m [llength $addErrors]
    echo "[kano] 4There w[s $m ere "as an"] error[s $m] while loading $m\
	    addon[s $m]: [listify $addErrors]" status
    set showinfo 1
}

if {$showinfo} {
    echo "[kano] 4Scroll up for more information on the errors" status
}

set title "welcome to the kano15 beta!"

set str {
                 $h1$title$h2
for giving you the privilege of having a release of kano
  which is not out to the majority of the public yet, I ask
  a few things in return
$b first off, please report any bugs you see -- ANY bugs you see
$i using the /bug function. for example, if you see a big "*** Error
$i in script kano.tcl: (...)", copy and paste the whole thing into
$i a bug report (type /bug then paste).
$b when reporting bugs, include your email address!!
$b spread this around to whomever wants it. it's okay.
$b if you have any questions, comments, or more extended bug reports,
$i please email kano@kano.net.
$b read the damn "what's new" file!! it's in your xircon\kano
$i directory
$b read the kanoMOTD whenever it's updated! (click knews://)
$b once again, use /bug to report ANYTHING, not just script errors.
$i if you have ANY ideas, problems, or comments on the script, use
$i /bug
$b enjoy!
 
please do NOT edit your copy of kano.tcl or any of the addons. 
if there's a feature you'd like to see in or out of the script or 
an addon, email me or use /bug.
}

if {[get_cookie kano_betaver] != [intvers]} {
    set_cookie kano_betaver [intvers]

    set h1 {}
    set h2 {}
    set b "- "
    set i "  "
    after idle [list msgbox $title [subst $str]]
}

set h1 Ü
set h2 Ü
set b "2þ "
set i "  "

echo [subst $str]

if {!$okmotd} exit
foreach i [concat status [queries] [channels]] {refresh $i}
onlinet
set LOADING 0