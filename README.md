# XiRCON + kano.tcl Setup on Apple Silicon

This guide shows you how to run the classic IRC client **XiRCON** with the **kano.tcl script** on modern Apple Silicon Macs using virtualization.

## About XiRCON & kano.tcl

**XiRCON** was one of the most popular IRC clients for Windows in the 1990s, known for its graphical user interface, ease of use, and exceptional scriptability using TCL. It was widely adopted across various fields including library helpdesk support, genealogy communities, and even US Naval command operations where it was more popular than mIRC. Development ceased in 1997 by author Mark Hanson.

**kano.tcl** is a comprehensive TCL script for XiRCON that provides "a complete set of tools for your everyday IRC needs along with a module and addon system for expandability." After XiRCON's discontinuation in 1997, it maintained "a steadily growing community based around the kano.tcl script" which extended the client's functionality significantly.

**Historical Resources:**
- [XiRCON Wikipedia Entry](https://en.wikipedia.org/wiki/XiRCON)
- [kano.tcl Script on SourceForge](https://sourceforge.net/projects/kanotcl/)
- [DareNET XiRCON Script Archives](https://archives.darenet.org/?dir=irc%2Fclients%2FXiRCON%2FScripts%2FKano+the+Xircon+Script)
- [Original kano.tcl homepage](https://web.archive.org/web/*/kano.net/kanotcl/) (Wayback Machine)

## Why This Guide?
With XiRCON being a 32-bit Windows application from the 1990s, it won't run natively on modern Apple Silicon Macs. Additionally, the original XiRCON and kano.tcl executables are increasingly difficult to find on the modern web, with many download links broken or dead. This guide provides a complete setup using Windows XP virtualization to get this classic IRC setup running on contemporary Apple hardware, with the hard-to-find executables included for convenience.

---

## Setup Instructions

1. Download UTM from [mac.getutm.app](https://mac.getutm.app)
2. Download Windows XP SP3 ISO from [https://archive.org/details/WinXPProSP3x86](https://archive.org/details/WinXPProSP3x86)
3. Download Windows XP template from [https://mac.getutm.app/gallery/windows-xp](https://mac.getutm.app/gallery/windows-xp)
4. Create VM with: Architecture x86_64, System i440FX, 512MB RAM, EFI disabled
5. **Network settings: Bridged mode + virtio-net-pci adapter**
6. Install SPICE guest tools from the Windows XP gallery page links
7. **Download software for transfer:**
  - [Firefox Setup 52.9.0esr.exe](https://ftp.mozilla.org/pub/firefox/releases/52.9.0esr/win32/en-US/) (last XP version)
  - [Notepad++ 7.9.2](https://github.com/notepad-plus-plus/notepad-plus-plus/releases/download/v7.9.2/npp.7.9.2.Installer.exe) (last XP version)
  - kano.tcl IRC client (included in this repo as `k15beta8.exe`)
  - XiRCON IRC client (included in this repo as `XiRC10b4.exe`)
8. **Create software ISO:** `hdiutil makehybrid -iso -joliet -o software.iso /path/to/folder/with/downloads`
9. Mount ISO in UTM CD drive and install all software

## Fix XiRCON Crashes
After installation, edit `C:\Program Files\XiRCON\kano\kano.tcl`:
1. Open the file in Notepad++
2. Press **Ctrl+G** to go to line 1601
3. Add a new line after line 1601 that says: `return`
4. Save the file

This prevents crashes from the expired MOTD domain.

