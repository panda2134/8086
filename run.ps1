$ENV:INCLUDE="C:\masm32\include;C:\Irvine"
$ENV:LIB="C:\masm32\lib;C:\Irvine"

if ($args.Count -ge 1) {
    Write-Output "Filename is $($args[0])"
    $filename=$($args[0]) 
} else { 
    $filename='.\emulator'
}

C:\masm32\bin\ml.exe /c /coff /Cp /Zd /Zf /Zi $filename".asm"
if (-not $?) {
    throw "ml.exe failed"
}
C:\masm32\bin\link.exe /SUBSYSTEM:CONSOLE /DEBUG $filename kernel32.lib user32.lib Irvine32.lib
if (-not $?) {
    throw "link.exe failed"
}

sudo C:\OllyDbg\OLLYDBG.exe $filename".exe"