Originally maya was a radical fork of clips but I realized that it was better
to keep the core of it the same and just work on creating a simple wrapper
around it instead. I got permission from work to open source the C++17 wrapper
library we use internally (LibElectron), this is different from the archived
LibElectron because this one is far more complete. To simplify the open
sourcing process, I have preserved the file structure we use internally for our
tools which use clips. We have designed the electron wrapper in such a way as
to make it more natural to interface with CLIPS from a C++17 context. 

I wrote the electron library for Parasoft starting in 2015 because we needed a better way to 
interface with CLIPS through Modern C++. All of the features in the library are used internally by
Parasoft. It started as a way so that we could use multiple environments in a single program in a very natural way. 
Since then it has evolved into providing more features like:

- Making it trivial and natural to call CLIPS functions from C++
- Making it simple to install and keep track of external address types automatically
- Allow CLIPS environments to be used as normal C++ objects
- Hide CLIPS API changes to the rest of our code
- Make it possible to have C-style include directives in CLIPS source code
- Expose features of boost to CLIPS
- Do not need to constantly update userfunctions.c to add new functionality
- Simplify per environment customization


Updating CLIPS
--------------
We designed Electron specifically so that we could easily update our version of CLIPS without requiring a massive rewrite
to any of the tools that use it. When we want to update CLIPS we just unpack a new version to the clips directory, 
make a small change to setup.h to introduce platform autodetect (not included here). After that point we run tests to 
make sure that the API has remained stable and executes as we expect.

The wrapper is meant for the CLIPS 64x series. 

