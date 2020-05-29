# csb-evolution

* This is a [Coding-Strike-Back ](https://www.codingame.com/multiplayer/bot-programming/coders-strike-back/) Simulator (Golden League & Legend League) 
* It also contains an advanced GA player that is currently ranked 70~80 at Legend League

---

Features : 
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

# Options

* ```-p1 <playerType> <name>``` define player1 (player2 can be defined in the same way)
* - ```playerType``` can be 
* - 1. Integer from 1~5 (default player from advance to simple)
* - 2. commad that you would type in shell to execute an external player executable file (ex. 'python3 player.py')
* - ```name``` is a string that will show in the animation to indicate the player (ex. boss1)

* ```-noAnimation ``` don't show animation , output an integer at stdout to indicate the winner instead.(For training players)
* ```-saveGame <savePath>``` provide a location to save the game information.
* ```-playFile <filePath>``` played the animaiton of a game saved earlier.

  
