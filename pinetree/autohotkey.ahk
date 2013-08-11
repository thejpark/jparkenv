; ===========================================================================
; Autohotkey configuration to define personal Windows shortcuts
; ===========================================================================

#i::Run "C:\Program Files\Google\Chrome\Application\chrome.exe"
#+i:: Run "C:\Program Files\Internet Explorer\iexplore.exe"

#o::Run "C:\Program Files\Microsoft Office\Office12\OUTLOOK.EXE"  /recycle
#+o::Run "C:\Program Files\OBJECTIVE\Navigator 7\Objective.exe"
;#s:: run C:\Program Files\PuTTYcyg\putty.exe -cygterm bash -l
#s:: run C:\cygwin\bin\mintty.exe -i /Cygwin-Terminal.ico -
#+^s:: run "C:\Program Files\Console2\Console.exe"
#+s:: run C:\Program Files\PuTTY\putty.exe -load rmdev

#e::Run "C:\Program Files\emacs-24.3\bin\runemacs.exe"
#+e::Run "C:\Program Files\emacs-24.3\bin\runemacs.exe" -Q

#+r::Reload
#w:: WinKill A

#m:: 
	WinGet MX, MinMax, A
	If MX
		WinRestore A
	Else WinMaximize A
	return

#right::
{
    WinID := WinActive("A")
    SysGet, Mon1, Monitor, 1
    SysGet, Mon2, Monitor, 2
    WinGetPos, WinX, WinY, WinWidth, , A

    WinCenter := WinX + (WinWidth / 2) ; Determines which monitor this is on by the position of the center pixel.
    if (WinCenter > Mon1Left and WinCenter < Mon1Right) {
        WinX := Mon2Left + (WinX - Mon1Left)
    } else if (WinCenter > Mon2Left and WinCenter < Mon2Right) {
        WinX := Mon1Left + (WinX - Mon2Left)
    }

    WinMove, ahk_id %WinID%, , %WinX%, %WinY%
    return
}
