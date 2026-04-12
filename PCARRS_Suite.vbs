Set WshShell = CreateObject("WScript.Shell")
Set fso = CreateObject("Scripting.FileSystemObject")

' Récupère le dossier où se trouve ce script VBS
strPath = fso.GetParentFolderName(WScript.ScriptFullName)

' --- LANCER LE BAT EN MODE VISIBLE (1) ---
' On utilise 1 pour que l'utilisateur voie la barre de progression de 20s
WshShell.Run chr(34) & strPath & "\run_app.bat" & chr(34), 1

Set WshShell = Nothing
Set fso = Nothing