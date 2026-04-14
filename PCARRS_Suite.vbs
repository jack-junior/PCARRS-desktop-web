Set WshShell = CreateObject("WScript.Shell")

' 1. Professional Welcome (4 seconds timeout)
WshShell.Popup "Initializing PCARRS Suite Engine..." & vbCrLf & "The interface will open shortly.", 4, "PCARRS Suite", 64

' 2. Launch the BAT file in Hidden Mode (0)
' The BAT will run all your checks and the animation in the background
WshShell.Run "cmd /c run_app.bat", 1, False

Set WshShell = Nothing