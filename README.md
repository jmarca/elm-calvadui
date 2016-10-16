# CalVAD map ui, Elm version

This is the CalVAD map user-interface, using
mostly [Elm](http://elm-lang.org/) apps and D3.

# seems to work

As of 2016-10-06, I have this up and running mostly, but for tweaking
and bug hunting

# running while developing

If developing, one item you have to do manually is link the
cellmembership json file so that the UI can get it from the static
file server:

After npm install, you should have node_modules/calvad_areas

then do

```
ln -sf node_modules/calvad_areas/lib/cellmembership.json src/static/grid/.
```
