# OgreQT5
*Qt5 Ogre3d integration*

Ogre3D Qt Window based on code from the [Ogre wiki](http://www.ogre3d.org/tikiwiki/tiki-index.php?page=Integrating+Ogre+into+QT5). Tested with Qt 5.17 and Ogre3D 1.13.0.
# Building
## Arch linux
*Tested on Arch linux with Qt 5.17 and both Ogre 1.13 and 1.10*

Then simply open the project in Qt Creator and build! In order to run
correctly, the working directory must be the same directory that
`plugins.cfg` is located in.

Changes from the original forked [version](https://github.com/RoryDungan/OgreQT5)

- Updated for deprecation of nodeless positioning -- added camera/light
  nodes.
- Fixed missing `{}` and updated formatting of the code. Removed `#if`
  conditions for versions and different OSes because I can't test different
  versions and don't want to keep the code I can't test around.
- Use absolute path to the `plugins.cfg` that is calculated from the
  `__FILE__` instead of requiring user to move it around.

