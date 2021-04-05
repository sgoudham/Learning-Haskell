sfOffice :: ([Char], [Char]) -> [Char]
sfOffice name = if surname < "L"
                then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
    where surname = snd name
          nameText = fst name ++ " " ++ surname

nyOffice :: ([Char], [Char]) -> [Char]
nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = fst name ++ " " ++ snd name

renoOffice :: (a, [Char]) -> [Char]
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
    where nameText = snd name

wdcOFfice :: ([Char], [Char]) -> [Char]
wdcOFfice name = nameText ++ " - PO BOX 831 - Washington, DC, 12130"
    where nameText = fst name ++ " " ++ snd name ++ ", Esq."


getLocation :: [Char] -> ([Char], [Char]) -> [Char]
getLocation location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "wdc" -> wdcOFfice
  _ -> (\name -> fst name ++ " " ++ snd name)


addressLetter :: ([Char], [Char]) -> [Char] -> [Char]
addressLetter name location = locationFunction name
  where locationFunction = getLocation location
