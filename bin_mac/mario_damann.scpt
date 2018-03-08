-----------------------------------------------------------------------
--Mario_Dammann is an applescript for the web browsers Firefox, Camino and Safari that copies selected text along with url and title in the same text file. At the end of the day you have a collection of all your selections  in one text file.

-- if you copy multiple texts segments from then same URL the segments are seperated by "... ..." and url and title won't be copied

--if TextEdit is closed or no TextEdit window is open Mario_Dammann opens it for you

-- put together from scripts of many great members of the applescript community by stephan schulz / www.maybevideodoes.de

-- make sure your textedit window is in front of all the other textedit windows
-----------------------------------------------------------------------

global theURL
global theTitle
global old_theURL
global madeNew
global MyApp

on fromWeb()
	--	display dialog MyApp
	
	set old_theURL to (the clipboard)
	if (MyApp is "Firefox") then
		tell application "Firefox"
			activate
			
			set theURL to Çclass curlÈ of window 1
			set theTitle to name of window 1
			tell application "System Events" to keystroke "c" using {command down}
		end tell
	end if
	if (MyApp is "Camino") then
		tell application "Camino"
			activate
			
			set theURL to URL of window 1
			set theTitle to name of window 1
			tell application "System Events" to keystroke "c" using {command down}
		end tell
	end if
	if (MyApp is "Safari") then
		tell application "Safari"
			activate
			
			set theURL to URL of front document
			set theTitle to name of front document
			tell application "System Events" to keystroke "c" using {command down}
		end tell
	end if
	
end fromWeb

on toText()
	tell application "System Events" to set texteditRunning to (name of processes) contains "TextEdit"
	
	tell application "TextEdit"
		
		if (exists of document 1) is false then
			make new document at end of documents
			set madeNew to true
		end if
		
		tell document 1
			if (theURL = old_theURL) is true and madeNew is false and texteditRunning is true then
				set oldText to its text
				set its text to oldText & "...   ..." & return & return & (the clipboard) & return & return
			else
				set oldText to its text
				set its text to oldText & "-----------------------------------------------------------------------" & return & Â
					theTitle & return Â
					& return & theURL & return Â
					& return & (the clipboard) & return & return
			end if
		end tell
		
	end tell
end toText

on frontApp()
	set front_app to (path to frontmost application as Unicode text)
	set AppleScript's text item delimiters to ":"
	set front_app to front_app's text items
	set AppleScript's text item delimiters to {""} --> restore delimiters to default value
	set item_num to (count of front_app) - 1
	
	set app_name to item item_num of front_app
	set AppleScript's text item delimiters to "."
	set app_name to app_name's text items
	set AppleScript's text item delimiters to {""} --> restore delimiters to default value
	--display dialog item 1 of app_name
	set MyApp to item 1 of app_name
end frontApp


on run
	set madeNew to false
	frontApp()
	fromWeb()
	toText()
	
	tell application MyApp to activate
	set the clipboard to theURL
end run