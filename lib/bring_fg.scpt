on run argv 
    try 
        Delay(0.5) 
        set proc to "" & item 1 of argv & "" 
        tell application "System Events" 
          tell process proc 
              set frontmost to true 
          end tell 
        end tell 
    on error 
    end try 
    return 
end run 