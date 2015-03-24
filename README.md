##Titim

This is a simple text based game about saving a village from falling letters.

Rules are as follows:

 - at every turn you enter a word
 - you can't use words more than once
 - every letter of an entered word is removed from the sky
 - if more than half of the houses are crushed you lose

<p align="center">
    <img src="http://i.imgur.com/FrUM2yh.png" alt="Example of the game on a terminal window"/>
</p>

####Installation

In order to build the executable and run it you need to download the Haskell platform and then
call the following commands:

```
cabal update
cabal install titim
titim
```
