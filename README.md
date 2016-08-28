# cl2048

Clone of popular 2048 game.

## Features
* GUI(Swing)
* game state saved automaticaly when you exit and restored when next
  time you start the game.
* hiscore

## Usage
You can run game with leiningen and source files or you can download binary jar file.

### Binary file
Download [jar](https://github.com/alexander982/cl2048/releases) file by double click on it. If it does not work on your system, you can run it from console.

    $ java -jar cl2048-0.1.0-standalone.jar

### Source
You need leiningen 2.0+ to build and/or run the game from source.
To run the game type:

    $ lein run

You can make an executable jar file with:

    $ lein uberjar

Then cd to target directory and type:

    $ java -jar cl2048-0.1.0-standalone.jar
