# cl2048

Clone of popular 2048 game.

## Features

* game state saved automaticaly when you exit and restored when next
  time you start the game.
* hiscore

## Usage

You need leiningen 2.0+ to build and/or run the game.
To run the game type:

    $ lein run

You can make an executable jar file with:

    $ lein uberjar

Then cd to target directory and type:

    $ java -jar cl2048-0.1.0-standalone.jar


