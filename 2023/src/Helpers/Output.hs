module Helpers.Output (clearScreen, clearLine) where
clearScreen = print "\033[2J"
clearLine = print "\033[2K\r"
