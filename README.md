##Titim

This is a simple text based game about saving a village from falling letters.
You start with a clean sky, and at every turn you are able to write a word that you haven't already
used, and every distinct letter in the word, will be removed from the sky.
If more than half of the houses get crushed by letters you lose.


<p align="center">
    <img src="http://i.imgur.com/FrUM2yh.png" alt="Example of the game on a terminal window"/>
</p>

####Build & run

In order to build the executable and run it you need to download the Haskell platform and then
call the following commands:

```
git clone https://github.com/Jefffrey/Titim.git
cd Titim
cabal update
cabal sandbox init
cabal configure
cabal install --only-dependencies
cabal build
cabal run -- <width> <height>
```
