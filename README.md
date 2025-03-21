# lightmap
a set of functions for haskell that make it easy to simulate 2d shadow casting.

This is another early Haskell project of mine. It stems from an old idea I had for a similar algorithm in Java.
It uses an algorithm which enumerates and labels the vertices of light-blocking walls by their angle from the light source, and measures intersects with rays going through these vertices. It's extremely fast and accurate, so would do well to be used to draw shadows in realtime; for example, in a videogame utilizing light-based mechanics, or a graphical physics simulation.

The file includes the main function, shadowshape, which takes in the source position and a list of walls and outputs the vertices of the shadow polgyon, as well as a demonstration function, doit, which uses the algorithm to draw and light a scene in the terminal. Take a look at its output here:
![TC0JlRu](https://user-images.githubusercontent.com/32168055/121476400-8529a400-c9be-11eb-8318-0ef39c3f4cc2.png)
