# Android Headset Media Controls for Windows
Lazarus/FreePascal application that hacks in support for Android headset media control for Windows.

This is a port of a python script found at [https://github.com/roligheten/AndroidMediaControlsWindows](https://github.com/roligheten/AndroidMediaControlsWindows)
For more details on how this works, see the associated [blog post](http://www.roligheten.no/blog/programming/2018/07/02/media-controls-windows.html)

Once started, application run as an icon in your tray. When a button in your headset is pressed (it can't detect which button is pressed) it emulate the press of the multimedia key "play/pause", even if your keyboard does not have one.
Source could be easily modified to call an external application or do other actions.

### Why I write this ###
I like listening music while I'm working in my notepad. Very often I use remote desktop to connect to various server, sometimes even nesting connections. So multimedia keys do not work and i need to go back to my pc to pause music.
Using this app is sufficient to press button on headset.


## Build
To build this application you will need:
*  [Free Pascal 3.0.4 ](http://www.freepascal.org)(last stable fixes branch) or above
*  [Lazarus 2.0](http://www.lazarus.freepascal.org) (current release candidate) or above

## Dependency
For audio monitoring this app need a binary build of [portaudio](http://portaudio.com/).
Recents build for major platforms could be find on the GitHub repository of [UOS](https://github.com/fredvs/uos/releases)
