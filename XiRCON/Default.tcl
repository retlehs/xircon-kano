#---------------------------------------------------------------------------
# Simple Aliases
#---------------------------------------------------------------------------
alias m {
    /msg [arg]
    complete
}

alias j {
    /join [arg]
    complete
}

alias p {
    /part [arg]
    complete
}

alias op {
    /mode [channel] +oooo [arg]
    complete
}

alias deop {
    /mode [channel] -oooo [arg]
    complete
}

alias ping {
    /ctcp [arg] ping
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
        /ctcp $c ACTION [arg]
        echo "[color nick]* [my_nick][color action] [arg]" channel $c
    }
    complete
}

alias onotice {
    set ops [nicks [channel] @*]
    set list ""
    if { [llength $ops] } {
        echo "[color notice]Ops-> [arg]"
        foreach op $ops {
            if { $op != "@[my_nick]" } {
                lappend list [string trimleft $op @]
                if { ![expr [llength $list] % 10] } {
                    set list [join $list ,]
                    /raw notice $list [arg]
                    set list ""
                }
            }
        }
        if { [llength $list] } {
            set list [join $list ,]
            /raw notice $list [arg]
        }
    }
    complete
}

alias run {
    if { [catch { exec [arg] & } msg] } {
        echo "*** $msg"
    }
    complete
}

alias clearall {
    window clear status
    foreach w [channels] {
        window clear channel $w
    }
    foreach w [queries] {
        window clear query $w
    }
    foreach w [chats] {
        window clear chat $w
    }
    complete
}

alias scripts {
    echo "Loaded scripts:"
    set i 0
    foreach s [scripts] {
        echo "\[[incr i]\] $s"
    }
    complete
}

alias tcl {
    if { [catch { eval [arg] } msg] } {
        echo "Error executing TCL command \"[join [args]]\" : "
        echo $msg
    }
    complete
}

alias splay {
    if { [llength [args]] } {
        if { ![stricmp [arg] "stop"] || ![stricmp [arg] "off"] } {
            mmstop all
        } else {
            set msg [mmplay [arg]]
            if { [string length $msg] } { echo "[color error]*** $msg" }
        }
    } else {
        echo "*** USAGE: /[event] <filename>"
    }
    complete
}

alias beep { beep; complete }


#---------------------------------------------------------------------------
# Ban Alias
#---------------------------------------------------------------------------
set ban_nicks ""
set ban_chans ""
set ban_types ""

alias ban {
    set chan [string tolower [channel]]
    set nick [string tolower [lindex [args] 0]]
    set type [lindex [args] 1]
    if { [am_on_channel $nick] } {
        set chan $nick
        set nick [string tolower [lindex [args] 1]]
        set type [lindex [args] 2]
    }
    if { ![string length $type] } { set type "5" }
    if { $nick != "" && $chan != "" } {
        lappend ban_nicks $nick
        lappend ban_chans $chan
        lappend ban_types $type
        whois $nick
    }
    complete
}

on whois {
    set nick [string tolower [nick]]
    set i [lsearch $ban_nicks $nick]
    while { $i > -1 } {
        set chan [lindex $ban_chans $i]
        set type [lindex $ban_types $i]
        set ban_nicks [lreplace $ban_nicks $i $i]
        set ban_chans [lreplace $ban_chans $i $i]
        set ban_types [lreplace $ban_types $i $i]
        /mode $chan +b [mask $type [nick]![user]@[host]]
        set i [lsearch $ban_nicks $nick]
    }
}

on 401 {
    set nick [string tolower [lindex [args] 1]]
    set i [lsearch $ban_nicks $nick]
    while { $i > -1 } {
        set ban_nicks [lreplace $ban_nicks $i $i]
        set ban_chans [lreplace $ban_chans $i $i]
        set ban_types [lreplace $ban_types $i $i]
        set i [lsearch $ban_nicks $nick]
    }
}


#---------------------------------------------------------------------------
# UserList-Related Aliases
#---------------------------------------------------------------------------
alias auser {
    set u [lindex [args] 0]
    if { [string length $u] } {
        set i [add_user [mask X $u]]
        echo "*** Added [get_user $i mask] at position $i" status
    } else {
        echo "*** USAGE: /[event] <mask>"
    }
    complete
}

alias duser {
    set u [lindex [args] 0]
    if { [string length $u] } {
        set i [find_user [mask X $u]]
        if { $i >= 0 } {
            set mask [get_user $i mask]
            delete_user $i
            echo "*** Deleted $mask"
        } else {
            echo "*** No users found matching [mask X $u]"
        }
    } else {
        echo "*** USAGE: /[event] <mask>"
    }
    complete
}


#---------------------------------------------------------------------------
# Domain Name Resolution Alias (DNS)
#---------------------------------------------------------------------------
set dns_nicks ""

alias dns {
    set host [arg]
    if { [string match *.* $host] } {
        lookup $host
        echo "*** Attempting to resolve $host" status
    } else {
        lappend dns_nicks [string tolower $host]
        whois $host
        echo "*** Retrieving information for $host" status
    }
    complete
}

on lookup {
    set request [lindex [args] 0]
    set result  [lindex [args] 1]
    if { [string length $result] } {
        echo "*** Resolved $request to $result" status
    } else {
        echo "[color error]*** ERROR: Could not resolve $request" status
    }
}

on whois {
    set nick [string tolower [nick]]
    set i [lsearch $dns_nicks $nick]
    while { $i > -1 } {
        lookup [host]
        echo "*** Attempting to resolve [host]" status
        set dns_nicks [lreplace $dns_nicks $i $i]
        set i [lsearch $dns_nicks $nick]
    }
}

on 401 {
    set nick [lindex [args] 1]
    set i [lsearch $dns_nicks $nick]
    while { $i > -1 } {
        set dns_nicks [lreplace $dns_nicks $i $i]
        set i [lsearch $dns_nicks $nick]
    }
}


#---------------------------------------------------------------------------
# Timer Alias
#---------------------------------------------------------------------------
set timers(0) ""
unset timers(0)

alias timer* {

    set id [string range [lindex [args] 0] 5 end]
    set args [lrange [args] 1 end]

    if { ![string length $id] || ![string compare [string tolower $id] s] } {
        set id 0
    }

    set off 0
    set beg 0

    set i [lsearch $args -o]
    if { $i < 0 } { set i [lsearch $args -O] }
    if { $i >= 0 } {
        set off 1
        set args [lreplace $args $i $i]
    }

    set i [lsearch $args *:*]
    if { $i >= 0 } {
        catch { set beg [clock scan [lindex $args $i]] }
        set args [lreplace $args $i $i]
    }

    set rep [lindex $args 0]
    set int [lindex $args 1]
    set cmd [join [lrange $args 2 end]]

    if { ![string compare [string tolower $rep] off] } {
        kill_timer $id
    } elseif { [llength $args] >= 3 } {
        make_timer $id $off $beg $rep $int $cmd
    } else {
        show_timers
    }

    complete
}

on timer {
    set names [array names timers]
    foreach id $names {
        process_timer $id
    }
}

proc make_timer { id off beg rep int cmd } {
    global timers
    if { ![string compare $id 0] } { set id [get_timer_id] }
    set timers($id) [list $off $beg $rep $int $cmd 0 0]
    echo "*** Timer$id activated"
}

proc kill_timer { id } {
    global timers
    if { [catch { unset timers($id) }] } {
        echo "*** Timer$id is not active"
    } else {
        echo "*** Timer$id halted"
    }
}

proc show_timers { } {
    global timers
    set names [array names timers]
    set count 0
    foreach id $names {
        set t $timers($id)
        set o "*** Timer$id"
        if { [lindex $t 1] > 0 } {
            append o " [clock format [lindex $t 1] -format %H:%M:%S]"
        }
        append o " [lindex $t 2]x"
        append o " [lindex $t 3]s"
        append o " [lindex $t 4]"
        echo $o
        incr count
    }
    if { !$count } { echo "*** No active timers" }
}

proc get_timer_id { } {
    global timers
    set i 1
    set names [array names timers]
    while { [lsearch $names $i] >= 0 } { incr i }
    return $i
}

proc process_timer { id } {
    global timers
    set ct  [clock seconds]
    set off [lindex $timers($id) 0]
    set beg [lindex $timers($id) 1]
    set rep [lindex $timers($id) 2]
    set int [lindex $timers($id) 3]
    set cmd [lindex $timers($id) 4]
    set lr  [lindex $timers($id) 5]
    set lt  [lindex $timers($id) 6]

    if { ![connected] && !$off } {
        return
    }
    if { $ct < $beg } {
        return
    }
    if { $ct < [expr $lt + $int] } {
        return
    }

    set beg 0

    incr lr

    set lt $ct

    eval $cmd

    if { $lr == $rep } {
        kill_timer $id
        return
    }

    set timers($id) [list $off $beg $rep $int $cmd $lr $lt]
}


#---------------------------------------------------------------------------
# Notify Events
#---------------------------------------------------------------------------
on notify {
    set msg ""
    append msg "[color notice]*** [color nick][nick] ([user]@[host])"
    append msg "[color notice] is on IRC"
    echo $msg status
}

on denotify {
    set msg ""
    append msg "[color notice]*** [color nick][nick] ([user]@[host])"
    append msg "[color notice] has left IRC"
    echo $msg status
}


#---------------------------------------------------------------------------
# Status Popup Menu
#---------------------------------------------------------------------------
menu status "*&Join #XiRCON" { /join #XiRCON }
menu status ""
menu status "&MOTD"          { /motd }
menu status ""
menu status "&Away"          { /away "since [clock format [clock seconds]]" }
menu status "&Back"          { /away }


#---------------------------------------------------------------------------
# Channel Popup Menu
#---------------------------------------------------------------------------
menu channel "*&Properties" { show_props channel [arg] }


#---------------------------------------------------------------------------
# Query Popup Menu
#---------------------------------------------------------------------------
menu query "*&Whois"         { /whois [arg] }
menu query ""
menu query "&CTCP->&Ping"    { /ctcp [arg] ping }
menu query "&CTCP->&Version" { /ctcp [arg] version }
menu query "&CTCP->&Time"    { /ctcp [arg] time }
menu query ""
menu query "&Finger"         { /finger [arg] }


#---------------------------------------------------------------------------
# Chat Popup Menu
#---------------------------------------------------------------------------
menu chat "*&Whois"         { /whois [arg] }
menu chat ""
menu chat "&CTCP->&Ping"    { /ctcp [arg] ping }
menu chat "&CTCP->&Version" { /ctcp [arg] version }
menu chat "&CTCP->&Time"    { /ctcp [arg] time }
menu chat ""
menu chat "&Finger"         { /finger [arg] }


#---------------------------------------------------------------------------
# Users Popup Menu
#---------------------------------------------------------------------------
menu users "&Whois" {
    foreach a [args] {
        /whois $a
    }
}
menu users "*&Query" {
    foreach a [args] {
        /query $a
    }
}
menu users "DCC Ch&at" {
    foreach a [args] {
        /dcc chat $a
    }
}
menu users ""
menu users "&CTCP->&Ping" {
    foreach a [args] {
        /ctcp $a ping
    }
}
menu users "&CTCP->&Version" {
    foreach a [args] {
        /ctcp $a version
    }
}
menu users "&CTCP->&Time" {
    foreach a [args] {
        /ctcp $a time
    }
}
menu users ""
menu users "&Op" {
    for { set i 0 } { $i < [llength [args]] } { incr i +4 } {
        /op [join [lrange [args] $i [expr $i + 3]]]
    }
}
menu users "&DeOp" {
    for { set i 0 } { $i < [llength [args]] } { incr i +4 } {
        /deop [join [lrange [args] $i [expr $i + 3]]]
    }
}
menu users ""
menu users "&Kick!" {
    foreach a [args] {
        /kick $a
    }
}
menu users "&Ban!" {
    foreach a [args] {
        /ban $a
    }
}
menu users ""
menu users "&Slap!" {
    foreach a [args] {
        /me slaps $a around a bit with a TCL powered popup
    }
}


#---------------------------------------------------------------------------
# Hotkeys
#---------------------------------------------------------------------------
hotkey alt+1 {
    window focus status
    status_page 0
    complete
}

hotkey alt+2 {
    window focus status
    status_page 1
    complete
}

hotkey alt+3 {
    window focus status
    status_page 2
    complete
}

hotkey alt+4 {
    window focus status
    status_page 3
    complete
}

hotkey alt+5 {
    window focus status
    status_page 4
    complete
}

hotkey control+t {
    /timestamp toggle
    complete
}


#---------------------------------------------------------------------------
# General Procedures
#---------------------------------------------------------------------------
proc arg {} {
    return [join [args]]
}

proc stricmp { s1 s2 } {
    return [string compare [string tolower $s1] [string tolower $s2]]
}

proc is_ip_addr { addr } {
    return [regexp {([0-9]+)\.([0-9]+)\.([0-9]+)\.([0-9]+)} $addr]
}

proc longip { ip } {
    global tcl_precision
    set tcl_precision 17
    set result 0
    regexp {([0-9]+)\.([0-9]+)\.([0-9]+)\.([0-9]+)} $ip s b3 b2 b1 b0
    if { ![string compare $ip $s] } {
        set total 0
        set total [expr $total + double($b3) * pow(256,3)]
        set total [expr $total + double($b2) * pow(256,2)]
        set total [expr $total + double($b1) * pow(256,1)]
        set total [expr $total + double($b0) * pow(256,0)]
        set result [format "%10.0f" $total]
    }
    return $result
}

proc mask { type mask } {
    set n "*"
    set u "*"
    set a "*"
    scan $mask "%\[^!\]!%\[^@\]@%s" n u a
    set n [string trimleft $n "@+"]
    set u [string trimleft $u "~"]
    set h $a
    set d ""
    if { [is_ip_addr $a] } {
        set a [split $a .]
        set a [lreplace $a end end *]
    } else {
        set a [split $a .]
        if { [llength $a] > 2 } { set a [lreplace $a 0 0 *] }
    }
    set d [join $a .]
    switch "$type" {
        "0" { return "*!$u@$h" }
        "1" { return "*!*$u@$h" }
        "2" { return "*!*@$h" }
        "3" { return "*!*$u@$d" }
        "4" { return "*!*@$d" }
        "5" { return "$n!$u@$h" }
        "6" { return "$n!*$u@$h" }
        "7" { return "$n!*@$h" }
        "8" { return "$n!*$u@$d" }
        "9" { return "$n!*@$d" }
    }
    return "$n!$u@$h"
}

proc am_on_channel { channel } {
    set c [string tolower $channel]
    set n [llength [channels]]
    for { set i 0 } { $i < $n } { incr i } {
        if { [string tolower [lindex [channels] $i]] == $c } {
            return 1
        }
    }
    return 0
}
