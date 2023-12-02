module Helpers.Output (clearScreen, clearLine) where
clearScreen :: IO ()
clearScreen = print "\033[2J"
clearLine :: IO ()
clearLine = print "\033[2K\r"
