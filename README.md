##Titim

This is a simple text based game about saving a village from falling letters.

Rules are as follows:

    - at every turn you enter a word
    - you can't use words more than once
    - every letter of an entered word is removed from the sky
    - if more than half of the houses are crushed you lose

The dictionary has its limits. If you find a better one you can change it, and make a pull request.

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

Feel free to change `dictionary.txt` with any dictionary you want, as long as the separator for each word is `\n`.
Size is configurable with command line arguments.
