-Download and install the engine, try the demos

If you haven't done it yet, download the engine source code with examples from ....

The example program that shows most of the features presented in this tutorial is examples/fps_game/ . I suggest looking at it's source code for a complete implementation that uses all the code snippets shown in this tutorial.

Run Lazarus, and open packages in the castle_game_engine/packages/ subdirectory. Open and compile all three packages (castle_base.lpk, castle_components.lpk, castle_window.lpk), to test that things compile OK. Then install the package <i>castle_components</i>. (Do not install castle_window --- you don't want to have this installed in Lazarus. You can install castle_base explicitly, but there's no need to: installing castle_components will also install castle_base automatically.)

Once packages are successfully installed, Lazarus restarts, and you should see "Castle" tab of components at the top (screenshot). Sorry, we don't have icons for our components yet, so it looks a little boring. Mouse over the icons to see component names.

Let's quickly open and run some demos, to make sure that everything works. I suggest running ... (some LCL demo) and ... (some CastleWindow demo).

Make sure you have installed the necessary libraries first, or some of the demos will not work. The required libraries (.so under Unix, .dll under Windows) are mentioned in the <a href="http://castle-engine.sourceforge.net/apidoc/html/introduction.html#SectionLibraries">Requirements -&gt; Libraries</a> section of our reference introduction. Under Windows, you will usually want to grab http://castle-engine.sourceforge.net/miscella/win32_dlls.zip and place them somewhere on your $PATH, or just place them in every directory with .exe files that you compile with our engine.

Now we'll start creating our own game from scratch.
