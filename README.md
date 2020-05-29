# csb-evolution

This is a Coding-Strike-Back Simulator (Golden League & Legend League)
The features includes : 
1. Simulating complete game between Players
2. Players can be external executable file (using online judge protocol) or haskell instance of PlayerIO (class defined in src/Player.hs) 
3. ```src/Player/```  contains several simple players and 2 advanced GA-based players
4. Game result can be Animated (enabled by default) 
5. Game result can be saved , and played as animation later.
# How to Use
 ```shell
 > git clone https://github.com/Nemo1999/csb-evolved.git
 > cd csb-evolved
 > stack init 
 > stack build 
```
Now , you can simulated game using
```shell
> stack exec -- csb-evolution-exec -RTS <options>
```
If no options are provided , it will animate a random game played by 2 GA players .
Currently , the animation start after the simulation of the entire game has finished , which may take up to 1~2 min if the players use the entire 75ms each turn.   
